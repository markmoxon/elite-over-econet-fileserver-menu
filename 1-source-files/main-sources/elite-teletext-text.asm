\ ******************************************************************************
\
\ TELETEXT ELITE TEXT ROUTINES
\
\ Elite was written by Ian Bell and David Braben and is copyright Acornsoft 1984
\
\ The code on this site has been reconstructed from a disassembly of the version
\ released on Ian Bell's personal website at http://www.elitehomepage.org/
\
\ The commentary is copyright Mark Moxon, and any misunderstandings or mistakes
\ in the documentation are entirely my fault
\
\ The terminology and notations used in this commentary are explained at
\ https://elite.bbcelite.com/terminology
\
\ The deep dive articles referred to in this commentary can be found at
\ https://elite.bbcelite.com/deep_dives
\
\ ******************************************************************************

\ ******************************************************************************
\
\       Name: ClearMode7Screen
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Clear the mode 7 screen
\
\ ******************************************************************************

.ClearMode7Screen

 LDA #0                 \ Set A = 0 so we can zero screen memory

 LDX #0                 \ Set a byte counter in X

.clrs1

 STA MODE7_VRAM,X       \ Zero the X-th byte of each of the four pages in a
 STA MODE7_VRAM+&100,X  \ mode 7 screen
 STA MODE7_VRAM+&200,X
 STA MODE7_VRAM+&300,X

 INX                    \ Increment the byte counter

 BNE clrs1              \ Loop back until we have counted a whole page

                        \ Fall into StyleTitleRow to style the title line as
                        \ appropriate

\ ******************************************************************************
\
\       Name: StyleTitleRow
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Print the control codes to style the first two lines of the screen
\
\ ******************************************************************************

.StyleTitleRow

 LDA #132               \ Style the top row as yellow text on blue background
 STA MODE7_VRAM
 LDA #157
 STA MODE7_VRAM+1
 LDA #131
 STA MODE7_VRAM+2

 LDA #151               \ Style the second row as white graphics
 STA MODE7_VRAM+(1*40)

 LDA #134               \ Style the rest of the screen as cyan text, returning
 JMP SetMode7Colour     \ from the subroutine using a tail call

\ ******************************************************************************
\
\       Name: SetMode7Graphics
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Insert a graphics control character at the start of row 2 onwards
\
\ ******************************************************************************

.SetMode7Graphics

 LDA #151               \ Set A to white graphics

\ ******************************************************************************
\
\       Name: SetMode7Colour
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Insert a control character at the start of row 2 onwards
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   A                   The control character to insert
\
\ ******************************************************************************

.SetMode7Colour

 FOR n, 2, 24
  STA MODE7_VRAM + n*40 \ Set rows 2 to 24 to the control character in A
 NEXT

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: ClearMenu
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Clear lines 16 to 24 in mode 7
\
\ ******************************************************************************

.ClearMenu

 LDA #0                 \ Set A = 0 so we can zero screen memory

 LDX #1                 \ Set a byte counter in X, starting from 1 so we skip
                        \ the graphics/text control character

.clyn1

 STA MODE7_VRAM+(16*40),X   \ Zero the X-th byte of rows 18 to 24
 STA MODE7_VRAM+(17*40),X
 STA MODE7_VRAM+(18*40),X
 STA MODE7_VRAM+(19*40),X
 STA MODE7_VRAM+(20*40),X
 STA MODE7_VRAM+(21*40),X
 STA MODE7_VRAM+(22*40),X
 STA MODE7_VRAM+(23*40),X
 STA MODE7_VRAM+(24*40),X

 INX                    \ Increment the byte counter

 CPX #40                \ Loop back until we have cleared the whole row
 BCC clyn1

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: SetShipColour
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Insert the graphics control character in A at the start of each
\             row in the ship view
\
\ ******************************************************************************

.SetShipColour

 FOR n, 1, 15
  STA MODE7_VRAM + n*40 \ Set rows 2 to 24 to the control character in A
 NEXT

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: ClearMode7Screen
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Clear the mode 7 screen
\
\ ******************************************************************************

.ClearShipScreen

 LDA #0                 \ Set A = 0 so we can zero screen memory

 LDX #1                 \ Set a byte counter in X, starting from 1 so we skip
                        \ the graphics/text control character

.clsp1

 STA MODE7_VRAM+(1*40),X   \ Zero the X-th byte of rows 1 to 15
 STA MODE7_VRAM+(2*40),X
 STA MODE7_VRAM+(3*40),X
 STA MODE7_VRAM+(4*40),X
 STA MODE7_VRAM+(5*40),X
 STA MODE7_VRAM+(6*40),X
 STA MODE7_VRAM+(7*40),X
 STA MODE7_VRAM+(8*40),X
 STA MODE7_VRAM+(9*40),X
 STA MODE7_VRAM+(10*40),X
 STA MODE7_VRAM+(11*40),X
 STA MODE7_VRAM+(12*40),X
 STA MODE7_VRAM+(13*40),X
 STA MODE7_VRAM+(14*40),X
 STA MODE7_VRAM+(15*40),X

 INX                    \ Increment the byte counter

 CPX #40                \ Loop back until we have cleared the whole row
 BCC clsp1

 RTS                    \ Return from the subroutine
