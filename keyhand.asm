; -----------------------------------------------------------------------------
; -KEYBOARD-HANDLING-----------------------------------------------------------
; -----------------------------------------------------------------------------


; on entry BC has last-k value in it. if there is a joystick movement,
; bc will be overwritten with the key code value associated with the direction
;
joytolastk:
   push  bc
   call  api_rdjoy
   pop   bc
   or    7
   cp    $ff
   ret   z                 ; return immediately if no bits clear

   ld    hl,joycodes       ; begin scanning
   ld    e,5

jt_scan:
   bit   7,a               ; zero bit indicates a press
   jr    nz,jt_nocode

   ld    c,(hl)            ; get the key code and return with it
   inc   hl
   ld    b,(hl)
   ret

jt_nocode:
   inc   hl                ; test next bit
   inc   hl
   sla   a
   dec   e
   jr    nz,jt_scan

   ret



; perform the keyboard handling. iterate over an array of structures defining a key mapping.
; each structure has a last-k value, state, counter and pointers to a type handler and 'on press' function.
;
keyhandler:
   ld      bc,(LAST_K)
   call    joytolastk

   ld      de,keyStates

   ; in short:
   ; compare LAST_K to each state's id. If they don't match,
   ; set that key's state to 0, else call the type handler.

kh_next:
   push  de                   ; points to data block

   ld    (kh_mod+1),de        ; [SMC] get key value from data block into de
kh_mod:
   ld    hl,(0)

   and   a                    ; last_k == required key val?
   sbc   hl,bc

   inc   de
   inc   de
   push  de                   ; point hl to the data part of the key code
   pop   hl

   jr    nz,kh_clearstate     ; check the result of the key data comparison - push/pop/inc don't affect flags

   inc   hl                   ; last-k matches
   inc   hl
   push  hl
   call  kh_vectorcall        ; call the updater, c will be set if we need to call the action function
   pop   hl
   inc   hl
   inc   hl
   call  c,kh_vectorcall      ; call the action/on-press function
   pop   de
   ret                        ; won't be any more matches

kh_clearstate:
   ; last_k doesn't match this key's required value, so set state and counter to 0.
   xor   a
   ld    (hl),a
   inc   hl
   ld    (hl),a

kh_advance:
   ld    a,8                  ; there are faster ways to do this but this call changes no registers
   call  stkAdd8
   pop   de

   ld    a,(de)               ; reached the end of the list? no key code has an lsb on 0.
   and   a
   jr    nz,kh_next

   ret

; take address at (hl) and _jump_ there; preserving hl
;
kh_vectorcall:
   push  hl                   ; store hl on the stack, it will be replaced later
   ld    a,(hl)               ; get the jump address into hl
   inc   hl
   ld    h,(hl)
   ld    l,a
   ex    (sp),hl              ; now put the jump address on the stack and recover the original hl
   ret                        ; then jump to the required address




; -----------------------------------------------------------------------------
; -KEY-TYPE-HANDLERS-----------------------------------------------------------
; -----------------------------------------------------------------------------

kType1:
   ; only call action when 1st pressed / one-shot
   ;
   ld    a,(de)
   and   a
   ret   nz                   ; return with carry clear if state != 1

   inc   a                    ; set state to 1 and set carry
   ld    (de),a
   ccf
   ret



kType2:
   ; simple fixed rate auto-repeat
   ;
   ld    a,(de)               ; get state
   ld    l,a                  ; preserve state for later comparison
   inc   a                    ; state = (state + 1) & 15.
   and   15
   ld    (de),a

   ld    a,l                  ; if state was 0 when we appeared here then trigger the call to action
   and   a                    ; clears carry
   ret   nz

   ccf                        ; fire a keypress when state == 0
   ret



kType3:
   ; initial action, then delay then auto repeat
   ;
   ld    (kt3_pt1+1),de
   ld    (kt3_done+1),de

kt3_pt1:
   ld    hl,(0)               ; [SMC]  get state into L and counter into H

   ld    a,h                  ; H will be 0 only on the 1st entry
   and   a
   jr    nz,kt3_not1sttime

   ld    l,-25                ; induce an initial delay
   ld    h,1
   ccf
   jr    kt3_done

kt3_not1sttime:
   inc   l
   ld    a,l
   cp    25
   jr    nz,kt3_done          ; carry will be clear at this point

   xor   a
   ld    l,a
   ccf

kt3_done:
   ld    (0),hl               ; [SMC] store state and counter
   ret





; -----------------------------------------------------------------------------
; -KEY-ACTION-HANDLERS---------------------------------------------------------
; -----------------------------------------------------------------------------



keyShift1:
   bit   0,(iy+QFLAGS)
   jr    z,ksc_set

   res   0,(iy+QFLAGS)
   ret

ksc_set:
   set   0,(iy+QFLAGS)
   ret


;
;
;


keyShiftEnter:
   bit   4,(iy+FFLAGS)        ; is current highlighted item a folder? return if not.
   ret   z

   ld    a,(FNBUF)
   cp    $9b                  ; '[.]' - nothing to do
   ret   z

   call  acceptpanechanges    ; copy the working pane back to the source

   ; work out which pane to copy. we won't copy all of it, only the screen offset and data ptr.
   ; once we have a pane info structure containing the other pane's basic info loaddir will
   ; fill in the rest with the updated file path.

   ld    a,(iy+CURPANE)
   xor   1
   ld    (iy+CURPANE),a

   call  getpaneptr           ; get pointer to pane source data

   ld    de,pnDATAPTR
   add   hl,de
   ld    de,DATAPTR           ; copy data to working pane
   ld    bc,4
   ldir

   call  updateFilePath

   call  gofast                ; set-fast

   call  loaddir
   call  acceptpanechanges
   call  drawlist

   ; swap back

   ld    a,(iy+CURPANE)
   xor   1
   ld    (iy+CURPANE),a
   push  af
   call  z,pane1
   pop   af
   call  nz,pane2

   jp    goslow                ; slow

;
;
;

keyEnter:
   bit   4,(iy+FFLAGS)        ; is current highlighted item a folder?
   jr    nz,ke_folder         ; forward if so to show subfolder content

   ld    hl,FNBUF
   ld    a,$1B                ; '.'
   call  findchar

   ; test that the extension is dot something terminator

   ret   nz                   ; no dot? no execute!

   inc   hl                   ; no terminator? no execute!
   inc   hl
   bit   7,(hl)
   ret   z

   dec   hl                   ; examine the extension itself

   ld    a,$35                ; 'P'
   cp    (hl)
   ret   nz

   ; change to the selected directory and execute the selected file

   ld    hl,DIRNAME
   ld    a,$12                ; '>' - change to the selected directory
   call  dircommand

   ld    hl,FNBUF             ; copy the selected filename up to filepath1 and set the high bit of the name
   jp    executeprog


ke_folder:
   ld    a,(FNBUF)
   cp    $9b                  ; '[.]' - nothing to do
   ret   z

   call  lolightitem

   call  updateFilePath

   call  gofast                ; set-fast

   call  loaddir
   call  acceptpanechanges

   call  drawlist
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   jp     goslow                ; slow

;
;
;

keyShiftLeftPress:
   bit   0,(iy+CURPANE)       ; nothing to do if already in pane 0
   ret   z

   call  acceptpanechanges
   call  lolightitem
   call  pane1
   call  parsefileinfo
   call  highlightitem
   call  drawdirectory
   jp    drawfile

;
;
;

keyShiftRightPress:
   bit   0,(iy+CURPANE)       ; nothing to do if already in pane 1
   ret   nz

   ld    a,(PANE2DATA+pnDIRNAME)
   and   a
   ret   z                    ; quit if dirname is not yet set

   call  acceptpanechanges
   call  lolightitem
   call  pane2
   call  parsefileinfo
   call  highlightitem
   call  drawdirectory
   jp    drawfile


;
;
;

keyQuit:
   call  $0a2a
   rst   08h
   .db   $ff

;
;
;

selectionUp:
   call  lolightitem       ; remove item highlighting

   ld    hl,SELECTION
   call  decINZ            ; move selection cursor up if possible
   jr    nz,su_done

   ld    hl,TOPENTRY
   call  decINZ            ; if cursor was already at top then try to decrement the list top item index
   jr    z,su_done         ; do nothing if it was already 0

   call  drawlist          ; if the top item changed then re-draw the list

su_done:
   call  acceptpanechanges
   call  highlightitem
   call  parsefileinfo
   jp    drawfile

;
;
;

selectionDown:
   call  lolightitem

   ld    hl,(SELECTION)       ; if the selection cursor is equal to num items then we can go no further
   ld    de,(NENTRIES)        ; this will happen when num items < 19
   dec   de
   and   a
   sbc   hl,de
   jr    z,sd_done

   ld    hl,(SELECTION)       ; if selection cursor is not at the bottom of the list...
   ld    a,l
   cp    NUMINLIST-1
   jr    z,sd_movelist

   inc   hl                   ;  ...then move it down
   ld    (SELECTION),hl
   jr    sd_done

sd_movelist:
   ld    de,(TOPENTRY)        ; otherwise if topentry + selection < num items, then move the list up.
   add   hl,de
   ld    de,(NENTRIES)
   dec   de
   sbc   hl,de
   jr    z,sd_done

   ld    de,(TOPENTRY)        ; shuffle the list up and re-draw it
   inc   de
   ld    (TOPENTRY),de
   call  drawlist

sd_done:
   call  acceptpanechanges
   call  highlightitem
   call  parsefileinfo
   jp    drawfile




adjustwindow:
   ld    hl,(TOPENTRY)        ; calculate the number of entries off the bottom of the pane display.
   ld    bc,18                ; = nentries - (topentry + 19)
   add   hl,bc
   ld    de,(NENTRIES)
   ex    de,hl
   sbc   hl,de                ; carry will be clear from previous addition
   jr    nc,aw_nover
   ld    hl,0                 ; 0 items remaining off-screen
aw_nover:
   ld    a,h
   or    l
   ret   nz                   ; nothing to do if there are items off the bottom of the pane

   ; we now know there's nothing off the bottom

   ld    hl,(TOPENTRY)        ; are there are any items off the top?
   ld    a,h
   or    l
   jr    z,aw_trybot

   ld    hl,TOPENTRY          ; shuffle list down from the top
   jp    decINZ

aw_trybot:
   ; we now know there's nothing above and nothing below.

   ; if the cursor is on the bottom item then move it up else do nothing

   ld    hl,(NENTRIES)
   ld    de,(SELECTION)
   and   a
   sbc   hl,de
   ret   nz

   ld    hl,SELECTION         ; move selection up
   jp    decINZ




keyDelete:
   ld    a,(FNBUF)            ; is current highlighted item a root folder?
   cp    $1b                  ; '.'
   ret   z

   call  lolightitem

   ld    hl,DIRNAME           ; copy the current directory name to FILEPATH1
   ld    de,FILEPATH1
   push  de
   call  createfilepath       ; then add current filename from FNBUF
   dec   hl                   ; hl points to terminator
   set   7,(hl)
   pop   de
   ld    a,1
   call  api_fileop

   call  gofast

   call  reloaddir
   call  acceptpanechanges
   call  adjustwindow
   call  drawlist
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   jp    goslow

;
;
;


keyCopy:
   ld    a,(iy+FFLAGS)           ; is current highlighted item a folder? return if not.
   and   $10
   ret   nz

   ld    a,(PANE2DATA+pnDIRNAME)
   and   a
   ret   z                    ; if pane 2 hasn't got a directory set then quit

   ; TODO quit if source and dest names are the same?

   ld    hl,$4000             ; source file too big?
   ld    de,(FLEN)
   and   a
   sbc   hl,de
   ret   c

   call  lolightitem

   ; create fqfn with ';32768' tagged on to the end.

   ld    hl,DIRNAME
   ld    de,FILEPATH1
   call  createfilepath       ; hl left pointing at terminator
   ld    de,sourcestr
   ex    de,hl
   call  copystrTHB

   ; create fqfn with ';32768,' tagged on to the end.

   ld    a,(iy+CURPANE)       ; get path from other pane
   xor   1
   call  getpaneptr
   ld    de,pnDIRNAME
   add   hl,de
   ld    de,FILEPATH2
   call  createfilepath       ; filepath2 contains destination pane directory plus filename
   ld    de,deststr
   ex    de,hl
   call  copystring           ; left pointing to the $ff byte. replace this with the size
   ld    (SCR_POS),de         ; by 'printing' the value there..!
   ld    hl,(FLEN)
   call  decimal16
   ld    hl,(SCR_POS)
   dec   hl
   set   7,(hl)

   call  $2e7                 ; really fast

   ld    de,FILEPATH1
   xor   a
   call  api_fileop

   ld    de,FILEPATH2
   ld    a,$ff
   call  api_fileop

   ; reload data for both panes - it's been trashed.

   call  acceptpanechanges

   call  reloadpanes
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   jp    $207              ; really slow

;
;
;

keyMove:
   bit   4,(iy+FFLAGS)           ; is current highlighted item a folder? return if so.
   ret   nz

   ld    a,(PANE2DATA+pnDIRNAME)
   and   a
   ret   z                    ; if pane 2 hasn't got a directory set then quit

   call  lolightitem

   ; create path 1
   ;
   ld    hl,DIRNAME
   ld    de,FILEPATH1
   call  createfilepath

   ; create path 2
   ;
   ld    a,(iy+CURPANE)    ; get path from other pane
   xor   1
   call  getpaneptr
   ld    de,pnDIRNAME
   add   hl,de
   ld    de,FILEPATH2
   call  createfilepath    ; add target path to string

   call  gofast

   call  rename
   cp    $40
   jr    nz,km_error

   ld    hl,NENTRIES       ; eeew, but necessary - need to check for errors though!!
   call  decINZ
   call  adjustwindow
   call  acceptpanechanges

   call  reloadpanes

km_error:
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   jp    goslow

;
;
;

keyRename:
   ld    a,(FNBUF)
   cp    $1b                  ; '.'
   ret   z

   call  lolightitem

   ld    hl,DIRNAME
   ld    de,FILEPATH1
   call  createfilepath       ; create fqfn

   ld    hl,FILEPATH1
   ld    de,FILEPATH2
   call  copystring           ; copy the path to the target

   call  bottombox
   ld    bc,$171a
   ld    hl,renamestr
   call  iprintstringat

   ld    bc,$1601             ; edit at SCR_POS
   call  pr_pos
   ld    hl,FILEPATH2
   call  editbuffer
   jr    z,kr_aborted

   ld    a,$18                ; '/' - see if the file moves between folders
   ld    hl,FILEPATH1
   call  count
   push  bc
   ld    hl,FILEPATH2
   call  count
   pop   hl
   ld    a,c
   sub   l
   ld    (DMFLAG),a

   call  gofast

   call  rename
   cp    $40
   jr    nz,kr_aborted        ; do nothing if this failed

   ld    a,(DMFLAG)           ; did the file change folders? don't adjust selection if not
   and   a
   jr    z,kr_onlyreload

   ld    hl,NENTRIES          ; eeew, but necessary - need to check for errors though!!
   call  decINZ
   call  adjustwindow
   call  acceptpanechanges

kr_onlyreload:
   call  reloadpanes

kr_aborted:
   call  unbottombox
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   jp    goslow


;
;
;

keyCreatedir:
   call  lolightitem

   call  bottombox
   ld    bc,$1710
   ld    hl,mkdirstr
   call  iprintstringat

   ld    bc,$1601             ; edit at SCR_POS
   call  pr_pos

   ld    hl,FNBUF             ; create a directory name
   ld    (hl),$ff
   call  editbuffer
   jr    z,kk_aborted

   ; TODO - refactor to use dircom

   ld    hl,FILEPATH1         ; concatenate the pane's directory and our new directory name
   ld    (hl),$15             ; '+'
   inc   hl
   ld    de,DIRNAME
   ex    de,hl
   call  createfilepath       ; dirname moved to filepath1+1 then has new dir added.

   dec   hl                   ; terminate and send
   set   7,(hl)
   ld    de,FILEPATH1
   call  api_sendstring

   ld    bc,$6007             ; open dir, which will interpret '+' command and create the directory
   ld    a,0
   out   (c),a
   call  api_responder

   cp    $40                  ; $40 = all good, else error. error rtn will fix up stack.
   jp    nz,error

   call  reloaddir
   call  acceptpanechanges
   call  drawlist

kk_aborted:
   call  unbottombox
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   jp    goslow

;
;
;

keyHelp:
   set   7,(iy+PRINTMOD)
   call  cls
   call  helpscreen
   res   7,(iy+PRINTMOD)

khl_wait:
   ld    a,(LAST_K)
   cp    $ff
   jr    nz,khl_wait
   ld    a,(LAST_K+1)
   cp    $ff
   jr    nz,khl_wait

khl_wait1:
   ld    a,(LAST_K)
   cp    $ff
   jr    z,khl_wait1
   ld    a,(LAST_K+1)
   cp    $ff
   jr    z,khl_wait1

   jp   drawscreen
