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
   call  kh_vectorcall        ; call the updater, c will be set if we need to call the action function
   inc   hl
   inc   hl
   call  c,kh_vectorcall      ; call the action/on-press function
   jr    kh_advance

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
   push  hl
   ld    a,(de)               ; get state
   ld    l,a                  ; preserve state for later comparison
   inc   a                    ; state = (state + 1) & 15.
   and   15
   ld    (de),a

   ld    a,l                  ; if state was 0 when we appeared here then trigger the call to action
   and   a                    ; clears carry
   jr    nz,kt2_wait

   ccf                        ; fire a keypress when state == 0

kt2_wait:
   pop   hl
   ret



kType3:
   ; initial action, then delay then auto repeat
   ;
   push  hl

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
   pop   hl
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

   push  hl
   push  bc
   push  de

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

   call  goslow                ; slow

   pop   de
   pop   bc
   pop   hl
   ret

;
;
;

keyEnter:
   bit   4,(iy+FFLAGS)        ; is current highlighted item a folder?
   jr    nz,ke_folder         ; forward if so to show subfolder content

   call  updateFilePath       ; otherwise execute the program
   jp    executeprog

ke_folder:
   ld    a,(FNBUF)
   cp    $9b                  ; '[.]' - nothing to do
   ret   z

   push  hl
   push  bc
   push  de

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

   call  goslow                ; slow

   pop   de
   pop   bc
   pop   hl
   ret

;
;
;

keyShiftLeftPress:
   bit   0,(iy+CURPANE)       ; nothing to do if already in pane 0
   ret   z

   push  hl
   push  bc
   push  de

   call  acceptpanechanges
   call  lolightitem
   call  pane1
   call  parsefileinfo
   call  highlightitem
   call  drawdirectory
   call  drawfile

   pop   de
   pop   bc
   pop   hl
   ret

;
;
;

keyShiftRightPress:
   bit   0,(iy+CURPANE)       ; nothing to do if already in pane 1
   ret   nz

   ld    a,(PANE2DATA+pnDIRNAME)
   and   a
   ret   z                    ; quit if dirname is not yet set

   push  hl
   push  bc
   push  de

   call  acceptpanechanges
   call  lolightitem
   call  pane2
   call  parsefileinfo
   call  highlightitem
   call  drawdirectory
   call  drawfile

   pop   de
   pop   bc
   pop   hl
   ret

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
   push  hl
   push  bc
   push  de
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
   call  drawfile

   pop   de
   pop   bc
   pop   hl
   ret

;
;
;

selectionDown:
   push  hl
   push  bc
   push  de
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
   call  drawfile

   pop   de
   pop   bc
   pop   hl
   ret






keyDelete:
   ld    a,(FNBUF)            ; is current highlighted item a root folder?
   cp    $1b                  ; '.'
   ret   z

   push  hl
   push  bc
   push  de
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
   call  drawlist
   ld    hl,SELECTION      ; decrement the selected item so we're not off the end of the list
   call  decINZ
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   call  goslow

   pop   de
   pop   bc
   pop   hl
   ret







keyCopy:
   ld    a,(iy+FFLAGS)           ; is current highlighted item a folder? return if not.
   and   $10
   ret   nz

   ld    a,(PANE2DATA+pnDIRNAME)
   and   a
   ret   z                    ; if pane 2 hasn't got a directory set then quit

   ; TODO quit if source and dest names are the same?

   push  hl
   push  bc
   push  de

   ld    hl,$4000             ; source file too big?
   ld    de,(FLEN)
   and   a
   sbc   hl,de
   jp    c,ksc_quit

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

   call  $207              ; really slow

ksc_quit:
   pop   de
   pop   bc
   pop   hl
   ret

;
;
;

rename:
   ; concatenate the paths using a semicolon
   ;
   ld    hl,FILEPATH1
   call  findend
   ld    a,$19             ; ';'
   ld    (hl),a
   inc   hl
   ld    de,FILEPATH2
   ex    de,hl
   call  copystrTHB

   ld    de,FILEPATH1
   call  api_sendstring

   ld    bc,$8007             ; execute rename command
   ld    a,$e0
   out   (c),a

   jp    api_responder

;
;
;

keyMove:
   bit   4,(iy+FFLAGS)           ; is current highlighted item a folder? return if so.
   ret   nz

   ld    a,(PANE2DATA+pnDIRNAME)
   and   a
   ret   z                    ; if pane 2 hasn't got a directory set then quit

   ; TODO quit if source and dest names are the same?

   push  hl
   push  bc
   push  de

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

   call  reloadpanes
   ld    hl,SELECTION      ; decrement the selected item so we're not off the end of the list
   call  decINZ
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   call  goslow

   pop   de
   pop   bc
   pop   hl
   ret

;
;
;

keyRename:
   ld    a,(FNBUF)
   cp    $1b                  ; '.'
   ret   z

   push  hl
   push  bc
   push  de
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
   push  af                   ; z flag will be clear if we changed directories

   call  gofast

   call  rename

   call  reloadpanes

   pop   af                   ; if the file moved, then move cursor up
   ld    hl,SELECTION
   call  nz,decINZ

kr_aborted:
   call  unbottombox
   call  highlightitem
   call  parsefileinfo
   call  drawdirectory
   call  drawfile

   call  goslow

   pop   de
   pop   bc
   pop   hl
   ret

;
;
;

keyCreatedir:
   push  hl
   push  bc
   push  de
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

   call  goslow

   pop   de
   pop   bc
   pop   hl
   ret

;
;
;

keyHelp:
   push  hl
   push  bc
   push  de

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

   call  drawscreen

   pop   de
   pop   bc
   pop   hl
   ret
