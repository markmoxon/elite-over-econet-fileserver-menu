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
\       Name: SetTextYellow
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Set yellow text in mode 7
\
\ ******************************************************************************

.SetTextYellow

 LDX #131               \ Set X to the "yellow text" control code

 EQUB &2C               \ Skip the next instruction, so we fall through into
                        \ PrintCharacter to print the control code

\ ******************************************************************************
\
\       Name: SetGraphicsWhite
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Set white graphics in mode 7
\
\ ******************************************************************************

.SetGraphicsWhite

 LDX #151               \ Set X to the "white graphics" control code

                        \ Fall through into PrintCharacter to print the control
                        \ code

\ ******************************************************************************
\
\       Name: PrintCharacter
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Print a teletext character at the current cursor
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   The teletext character to print
\
\ ******************************************************************************

.PrintCharacter

 LDA YC                 \ Fetch YC, the y-coordinate (row) of the text cursor

 CMP #25                \ If the character is off the bottom of the screen, jump
 BCS prin1              \ to prin1 to return from the subroutine

 ASL A                  \ Add the address for the start of row YC (from the
 TAY                    \ charRowAddress table) to SC to give the screen
 LDA charRowAddress,Y   \ address of the character we want to print
 STA P
 LDA charRowAddress+1,Y
 STA P+1

 LDY XC                 \ Fetch XC, x-coordinate (column) of the text cursor

 CPY #40                \ If the character is off to the right of the screen,
 BCS prin1              \ jump to prin1 to return from the subroutine

 TXA                    \ Store the character given in X in the XC-th character
 STA (P),Y              \ on the row at SC(1 0)

 INC XC                 \ Move the text cursor to the right by one character

.prin1

 RTS                    \ Return from the subroutine

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

 BIT displayTitle       \ If bit 7 of displayTitle is set, jump to stit1 to skip
 BMI stit1              \ displaying the title row

 LDA QQ11               \ If this is not the death screen, jump to stit2 to
 CMP #6                 \ display the title row
 BNE stit2

.stit1

 LDA #151               \ This is either the death screen or bit 7 of
 STA MODE7_VRAM         \ displayTitle is set, so style the top row as white
                        \ graphics

 BNE stit4              \ Jump to stit4 to style the second row as white
                        \ graphics (this BNE is effectively a JMP as A is never
                        \ zero)

.stit2

                        \ Galfax code removed as it isn't needed

.stit3

 LDA #132               \ Style the top row as yellow text on blue background
 STA MODE7_VRAM
 LDA #157
 STA MODE7_VRAM+1
 LDA #131
 STA MODE7_VRAM+2

.stit4

 LDA #151               \ Style the second row as white graphics
 STA MODE7_VRAM+(1*40)

 LDA #134               \ Style the rest of the screen as cyan text, returning
 JMP SetMode7Colour     \ from the subroutine using a tail call


\ ******************************************************************************
\
\       Name: displayTitle
\       Type: Variable
\   Category: Teletext Elite
\    Summary: Flag to control whether to display a blue title row at the top of
\             the screen
\
\ ******************************************************************************

.displayTitle

 EQUB 0                 \ Determines whether to draw a blue title row:
                        \
                        \   * Bit 7 clear = draw a blue title row
                        \
                        \   * Bit 7 set = do not draw a blue title row

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

