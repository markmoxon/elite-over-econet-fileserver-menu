\ ******************************************************************************
\
\ ELITE OVER ECONET SERVER MENU SOURCE (Part 1)
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
\ ------------------------------------------------------------------------------
\
\ This source file produces the following binary file:
\
\   * MENU.bin
\
\ ******************************************************************************

 INCLUDE "1-source-files/main-sources/elite-build-options.asm"

 _IB_DISC               = (_VARIANT = 1)
 _STH_DISC              = (_VARIANT = 2)
 _SRAM_DISC             = (_VARIANT = 3)

 GUARD &6000            \ Guard against assembling over screen memory

 _DOCKED = TRUE         \ Set compilation flag for docked vs flight code

 _LOADER = TRUE         \ Set compilation flag for loader code

 INCLUDE "1-source-files/main-sources/elite-teletext-macros.asm"

\ ******************************************************************************
\
\ Configuration variables
\
\ ******************************************************************************

 CODE% = &3000          \ The address where the code will be run

 LOAD% = &3000          \ The address where the code will be loaded

 N% = 12                \ N% is set to the number of bytes in the VDU table, so
                        \ we can loop through them below

 VIA = &FE00            \ Memory-mapped space for accessing internal hardware,
                        \ such as the video ULA, 6845 CRTC and 6522 VIAs (also
                        \ known as SHEILA)

 OSWRCH = &FFEE         \ The address for the OSWRCH routine

 OSBYTE = &FFF4         \ The address for the OSBYTE routine

 OSCLI = &FFF7          \ The address for the OSCLI vector

\ ******************************************************************************
\
\       Name: ZP
\       Type: Workspace
\    Address: &0070 to &008B
\   Category: Workspaces
\    Summary: Important variables used by the loader
\
\ ******************************************************************************

 ORG &0070

.ZP

 SKIP 2                 \ Stores addresses used for moving content around

.P

 SKIP 1                 \ Temporary storage, used in a number of places

.Q

 SKIP 1                 \ Temporary storage, used in a number of places

.YY

 SKIP 1                 \ Temporary storage, used in a number of places

.T

 SKIP 1                 \ Temporary storage, used in a number of places

.R

 SKIP 2                 \ Temporary storage, used in the mode 7 screen routines

\ ******************************************************************************
\
\ ELITE LOADER
\
\ ******************************************************************************

 ORG CODE%

\ ******************************************************************************
\
\       Name: B%
\       Type: Variable
\   Category: Drawing the screen
\    Summary: VDU commands for setting the mode 7 screen
\
\ ******************************************************************************

.B%

 EQUB 22, 7             \ Switch to screen mode 7

 EQUB 23, 0, 10, 32     \ Set 6845 register R10 = %00100000 = 32
 EQUB 0, 0, 0           \
 EQUB 0, 0, 0           \ This is the "cursor start" register, and bits 5 and 6
                        \ define the "cursor display mode", as follows:
                        \
                        \   * %00 = steady, non-blinking cursor
                        \
                        \   * %01 = do not display a cursor
                        \
                        \   * %10 = fast blinking cursor (blink at 1/16 of the
                        \           field rate)
                        \
                        \   * %11 = slow blinking cursor (blink at 1/32 of the
                        \           field rate)
                        \
                        \ We can therefore turn off the cursor completely by
                        \ setting cursor display mode %01, with bit 6 of R10
                        \ clear and bit 5 of R10 set

\ ******************************************************************************
\
\       Name: Elite loader
\       Type: Subroutine
\   Category: Loader
\    Summary: Set up mode 7, draw Saturn and run the second menu program
\
\ ******************************************************************************

.ENTRY

 LDA #144               \ Call OSBYTE with A = 144, X = 255 and Y = 0 to move
 LDX #255               \ the screen down one line and turn screen interlace on
 JSR OSB

 LDA #LO(B%)            \ Set the low byte of ZP(1 0) to point to the VDU code
 STA ZP                 \ table at B%

 LDA #HI(B%)            \ Set the high byte of ZP(1 0) to point to the VDU code
 STA ZP+1               \ table at B%

 LDY #0                 \ We are now going to send the N% VDU bytes in the table
                        \ at B% to OSWRCH to set up the special mode 4 screen
                        \ that forms the basis for the split-screen mode

.loop1

 LDA (ZP),Y             \ Pass the Y-th byte of the B% table to OSWRCH
 JSR OSWRCH

 INY                    \ Increment the loop counter

 CPY #N%                \ Loop back for the next byte until we have done them
 BNE loop1              \ all (the number of bytes was set in N% above)

 JSR DrawSaturn         \ Call DrawSaturn to draw a mode 7 Saturn

 LDX #LO(RDLI)          \ Set (Y X) to point to RDLI ("RUN MENU2")
 LDY #HI(RDLI)

 JMP OSCLI              \ Call OSCLI to run the OS command in RDLI, which runs
                        \ the MENU2 binary

.RDLI

 EQUS "RUN MENU2"
 EQUB 13

\ ******************************************************************************
\
\       Name: PLL1 (Part 1 of 3)
\       Type: Subroutine
\   Category: Drawing planets
\    Summary: Draw Saturn on the loading screen (draw the planet)
\  Deep dive: Drawing Saturn on the loading screen
\
\ ******************************************************************************

.PLL1

                        \ The following loop iterates CNT(1 0) times, i.e. &300
                        \ or 768 times, and draws the planet part of the
                        \ loading screen's Saturn

 LDA VIA+&44            \ Read the 6522 System VIA T1C-L timer 1 low-order
 STA RAND+1             \ counter (SHEILA &44), which decrements one million
                        \ times a second and will therefore be pretty random,
                        \ and store it in location RAND+1, which is among the
                        \ main game code's random seeds in RAND (so this seeds
                        \ the random number generator)

 JSR DORND              \ Set A and X to random numbers, say A = r1

 JSR SQUA2              \ Set (A P) = A * A
                        \           = r1^2

 STA ZP+1               \ Set ZP(1 0) = (A P)
 LDA P                  \             = r1^2
 STA ZP

 JSR DORND              \ Set A and X to random numbers, say A = r2

 STA YY                 \ Set YY = A
                        \        = r2

 JSR SQUA2              \ Set (A P) = A * A
                        \           = r2^2

 TAX                    \ Set (X P) = (A P)
                        \           = r2^2

 LDA P                  \ Set (A ZP) = (X P) + ZP(1 0)
 ADC ZP                 \
 STA ZP                 \ first adding the low bytes

 TXA                    \ And then adding the high bytes
 ADC ZP+1

 BCS PLC1               \ If the addition overflowed, jump down to PLC1 to skip
                        \ to the next pixel

 STA ZP+1               \ Set ZP(1 0) = (A ZP)
                        \             = r1^2 + r2^2

 LDA #1                 \ Set ZP(1 0) = &4001 - ZP(1 0) - (1 - C)
 SBC ZP                 \             = 128^2 - ZP(1 0)
 STA ZP                 \
                        \ (as the C flag is clear), first subtracting the low
                        \ bytes

 LDA #&40               \ And then subtracting the high bytes
 SBC ZP+1
 STA ZP+1

 BCC PLC1               \ If the subtraction underflowed, jump down to PLC1 to
                        \ skip to the next pixel

                        \ If we get here, then both calculations fitted into
                        \ 16 bits, and we have:
                        \
                        \   ZP(1 0) = 128^2 - (r1^2 + r2^2)
                        \
                        \ where ZP(1 0) >= 0

 JSR ROOT               \ Set ZP = SQRT(ZP(1 0))

 LDA ZP                 \ Set X = ZP >> 1
 LSR A                  \       = SQRT(128^2 - (a^2 + b^2)) / 2
 TAX

 LDA YY                 \ Set A = YY
                        \       = r2

 CMP #128               \ If YY >= 128, set the C flag (so the C flag is now set
                        \ to bit 7 of A)

 ROR A                  \ Rotate A and set the sign bit to the C flag, so bits
                        \ 6 and 7 are now the same, i.e. A is a random number in
                        \ one of these ranges:
                        \
                        \   %00000000 - %00111111  = 0 to 63    (r2 = 0 - 127)
                        \   %11000000 - %11111111  = 192 to 255 (r2 = 128 - 255)
                        \
                        \ The PIX routine flips bit 7 of A before drawing, and
                        \ that makes -A in these ranges:
                        \
                        \   %10000000 - %10111111  = 128-191
                        \   %01000000 - %01111111  = 64-127
                        \
                        \ so that's in the range 64 to 191

 JSR PIX                \ Draw a pixel at screen coordinate (X, -A), i.e. at
                        \
                        \   (ZP / 2, -A)
                        \
                        \ where ZP = SQRT(128^2 - (r1^2 + r2^2))
                        \
                        \ So this is the same as plotting at (x, y) where:
                        \
                        \   r1 = random number from 0 to 255
                        \   r2 = random number from 0 to 255
                        \   (r1^2 + r2^2) < 128^2
                        \
                        \   y = r2, squished into 64 to 191 by negation
                        \
                        \   x = SQRT(128^2 - (r1^2 + r2^2)) / 2
                        \
                        \ which is what we want

.PLC1

 DEC CNT                \ Decrement the counter in CNT (the low byte)

 BNE PLL1               \ Loop back to PLL1 until CNT = 0

 DEC CNT+1              \ Decrement the counter in CNT+1 (the high byte)

 BNE PLL1               \ Loop back to PLL1 until CNT+1 = 0

\ ******************************************************************************
\
\       Name: PLL1 (Part 2 of 3)
\       Type: Subroutine
\   Category: Drawing planets
\    Summary: Draw Saturn on the loading screen (draw the stars)
\  Deep dive: Drawing Saturn on the loading screen
\
\ ******************************************************************************

                        \ The following loop iterates CNT2(1 0) times, i.e. &1DD
                        \ or 477 times, and draws the background stars on the
                        \ loading screen

.PLL2

 JSR DORND              \ Set A and X to random numbers, say A = r3

 TAX                    \ Set X = A
                        \       = r3

 JSR SQUA2              \ Set (A P) = A * A
                        \           = r3^2

 STA ZP+1               \ Set ZP+1 = A
                        \          = r3^2 / 256

 JSR DORND              \ Set A and X to random numbers, say A = r4

 STA YY                 \ Set YY = r4

 JSR SQUA2              \ Set (A P) = A * A
                        \           = r4^2

 ADC ZP+1               \ Set A = A + r3^2 / 256
                        \       = r4^2 / 256 + r3^2 / 256
                        \       = (r3^2 + r4^2) / 256

 CMP #&11               \ If A < 17, jump down to PLC2 to skip to the next pixel
 BCC PLC2

 LDA YY                 \ Set A = r4

 JSR PIX                \ Draw a pixel at screen coordinate (X, -A), i.e. at
                        \ (r3, -r4), where (r3^2 + r4^2) / 256 >= 17
                        \
                        \ Negating a random number from 0 to 255 still gives a
                        \ random number from 0 to 255, so this is the same as
                        \ plotting at (x, y) where:
                        \
                        \   x = random number from 0 to 255
                        \   y = random number from 0 to 255
                        \   (x^2 + y^2) div 256 >= 17
                        \
                        \ which is what we want

.PLC2

 DEC CNT2               \ Decrement the counter in CNT2 (the low byte)

 BNE PLL2               \ Loop back to PLL2 until CNT2 = 0

 DEC CNT2+1             \ Decrement the counter in CNT2+1 (the high byte)

 BNE PLL2               \ Loop back to PLL2 until CNT2+1 = 0

\ ******************************************************************************
\
\       Name: PLL1 (Part 3 of 3)
\       Type: Subroutine
\   Category: Drawing planets
\    Summary: Draw Saturn on the loading screen (draw the rings)
\  Deep dive: Drawing Saturn on the loading screen
\
\ ******************************************************************************

                        \ The following loop iterates CNT3(1 0) times, i.e. &333
                        \ or 819 times, and draws the rings around the loading
                        \ screen's Saturn

.PLL3

 JSR DORND              \ Set A and X to random numbers, say A = r5

 STA ZP                 \ Set ZP = r5

 JSR SQUA2              \ Set (A P) = A * A
                        \           = r5^2

 STA ZP+1               \ Set ZP+1 = A
                        \          = r5^2 / 256

 JSR DORND              \ Set A and X to random numbers, say A = r6

 STA YY                 \ Set YY = r6

 JSR SQUA2              \ Set (A P) = A * A
                        \           = r6^2

 STA T                  \ Set T = A
                        \       = r6^2 / 256

 ADC ZP+1               \ Set ZP+1 = A + r5^2 / 256
 STA ZP+1               \          = r6^2 / 256 + r5^2 / 256
                        \          = (r5^2 + r6^2) / 256

 LDA ZP                 \ Set A = ZP
                        \       = r5

 CMP #128               \ If A >= 128, set the C flag (so the C flag is now set
                        \ to bit 7 of ZP, i.e. bit 7 of A)

 ROR A                  \ Rotate A and set the sign bit to the C flag, so bits
                        \ 6 and 7 are now the same

 CMP #128               \ If A >= 128, set the C flag (so again, the C flag is
                        \ set to bit 7 of A)

 ROR A                  \ Rotate A and set the sign bit to the C flag, so bits
                        \ 5-7 are now the same, i.e. A is a random number in one
                        \ of these ranges:
                        \
                        \   %00000000 - %00011111  = 0-31
                        \   %11100000 - %11111111  = 224-255
                        \
                        \ In terms of signed 8-bit integers, this is a random
                        \ number from -32 to 31. Let's call it r7

 ADC YY                 \ Set A = A + YY
                        \       = r7 + r6

 TAX                    \ Set X = A
                        \       = r6 + r7

 JSR SQUA2              \ Set (A P) = A * A
                        \           = (r6 + r7)^2

 TAY                    \ Set Y = A
                        \       = (r6 + r7)^2 / 256

 ADC ZP+1               \ Set A = A + ZP+1
                        \       = (r6 + r7)^2 / 256 + (r5^2 + r6^2) / 256
                        \       = ((r6 + r7)^2 + r5^2 + r6^2) / 256

 BCS PLC3               \ If the addition overflowed, jump down to PLC3 to skip
                        \ to the next pixel

 CMP #80                \ If A >= 80, jump down to PLC3 to skip to the next
 BCS PLC3               \ pixel

 CMP #32                \ If A < 32, jump down to PLC3 to skip to the next pixel
 BCC PLC3

 TYA                    \ Set A = Y + T
 ADC T                  \       = (r6 + r7)^2 / 256 + r6^2 / 256
                        \       = ((r6 + r7)^2 + r6^2) / 256

 CMP #16                \ If A >= 16, skip to PL1 to plot the pixel
 BCS PL1

 LDA ZP                 \ If ZP is positive (i.e. r5 < 128), jump down to PLC3
 BPL PLC3               \ to skip to the next pixel

.PL1

                        \ If we get here then the following is true:
                        \
                        \   32 <= ((r6 + r7)^2 + r5^2 + r6^2) / 256 < 80
                        \
                        \ and either this is true:
                        \
                        \   ((r6 + r7)^2 + r6^2) / 256 >= 16
                        \
                        \ or both these are true:
                        \
                        \   ((r6 + r7)^2 + r6^2) / 256 < 16
                        \   r5 >= 128

 LDA YY                 \ Set A = YY
                        \       = r6

 JSR PIX                \ Draw a pixel at screen coordinate (X, -A), where:
                        \
                        \   X = (random -32 to 31) + r6
                        \   A = r6
                        \
                        \ Negating a random number from 0 to 255 still gives a
                        \ random number from 0 to 255, so this is the same as
                        \ plotting at (x, y) where:
                        \
                        \   r5 = random number from 0 to 255
                        \   r6 = random number from 0 to 255
                        \   r7 = r5, squashed into -32 to 31
                        \
                        \   x = r6 + r7
                        \   y = r6
                        \
                        \   32 <= ((r6 + r7)^2 + r5^2 + r6^2) / 256 < 80
                        \
                        \   Either: ((r6 + r7)^2 + r6^2) / 256 >= 16
                        \
                        \   Or:     ((r6 + r7)^2 + r6^2) / 256 <  16
                        \           r5 >= 128
                        \
                        \ which is what we want

.PLC3

 DEC CNT3               \ Decrement the counter in CNT3 (the low byte)

 BNE PLL3               \ Loop back to PLL3 until CNT3 = 0

 DEC CNT3+1             \ Decrement the counter in CNT3+1 (the high byte)

 BNE PLL3               \ Loop back to PLL3 until CNT3+1 = 0

\ ******************************************************************************
\
\       Name: DORND
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Generate random numbers
\  Deep dive: Generating random numbers
\             Fixing ship positions
\
\ ------------------------------------------------------------------------------
\
\ Set A and X to random numbers (though note that X is set to the random number
\ that was returned in A the last time DORND was called).
\
\ The C and V flags are also set randomly.
\
\ This is a simplified version of the DORND routine in the main game code. It
\ swaps the two calculations around and omits the ROL A instruction, but is
\ otherwise very similar. See the DORND routine in the main game code for more
\ details.
\
\ ******************************************************************************

.DORND

 LDA RAND+1             \ r1´ = r1 + r3 + C
 TAX                    \ r3´ = r1
 ADC RAND+3
 STA RAND+1
 STX RAND+3

 LDA RAND               \ X = r2´ = r0
 TAX                    \ A = r0´ = r0 + r2
 ADC RAND+2
 STA RAND
 STX RAND+2

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: RAND
\       Type: Variable
\   Category: Drawing planets
\    Summary: The random number seed used for drawing Saturn
\
\ ******************************************************************************

.RAND

 EQUD &34785349

\ ******************************************************************************
\
\       Name: SQUA2
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A P) = A * A
\  Deep dive: Shift-and-add multiplication
\
\ ------------------------------------------------------------------------------
\
\ Do the following multiplication of signed 8-bit numbers:
\
\   (A P) = A * A
\
\ This uses a similar approach to routine SQUA2 in the main game code, which
\ itself uses the MU11 routine to do the multiplication. However, this version
\ first ensures that A is positive, so it can support signed numbers.
\
\ ******************************************************************************

.SQUA2

 BPL SQUA               \ If A > 0, jump to SQUA

 EOR #&FF               \ Otherwise we need to negate A for the SQUA algorithm
 CLC                    \ to work, so we do this using two's complement, by
 ADC #1                 \ setting A = ~A + 1

.SQUA

 STA Q                  \ Set Q = A and P = A

 STA P                  \ Set P = A

 LDA #0                 \ Set A = 0 so we can start building the answer in A

 LDY #8                 \ Set up a counter in Y to count the 8 bits in P

 LSR P                  \ Set P = P >> 1
                        \ and C flag = bit 0 of P

.SQL1

 BCC SQ1                \ If C (i.e. the next bit from P) is set, do the
 CLC                    \ addition for this bit of P:
 ADC Q                  \
                        \   A = A + Q

.SQ1

 ROR A                  \ Shift A right to catch the next digit of our result,
                        \ which the next ROR sticks into the left end of P while
                        \ also extracting the next bit of P

 ROR P                  \ Add the overspill from shifting A to the right onto
                        \ the start of P, and shift P right to fetch the next
                        \ bit for the calculation into the C flag

 DEY                    \ Decrement the loop counter

 BNE SQL1               \ Loop back for the next bit until P has been rotated
                        \ all the way

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: PIX
\       Type: Subroutine
\   Category: Drawing pixels
\    Summary: Draw a single pixel at a specific coordinate
\
\ ------------------------------------------------------------------------------
\
\ Draw a pixel at screen coordinate (X, -A). The sign bit of A gets flipped
\ before drawing, and then the routine uses the same approach as the PIXEL
\ routine in the main game code, except it plots a single pixel from TWOS
\ instead of a two pixel dash from TWOS2. This applies to the top part of the
\ screen (the monochrome mode 4 space view).
\
\ See the PIXEL routine in the main game code for more details.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   The screen x-coordinate of the pixel to draw
\
\   A                   The screen y-coordinate of the pixel to draw, negated
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   out                 Contains an RTS
\
\ ******************************************************************************

.PIX

 EOR #%10000000         \ Flip the sign of A

 SCALE_SIXEL_Y          \ Scale the pixel y-coordinate in A into sixels

 TAY                    \ Copy the sixel y-coordinate to Y

 TXA                    \ Copy the pixel x-coordinate to A

 CLC                    \ The origin for the x-coordinate is in the centre of
 ADC #128               \ the screen, so add 128 so the origin is on the left
                        \ of the screen, ready for the PlotSixelClipped routine

 SCALE_SIXEL_X          \ Scale the pixel x-coordinate in A into sixels

 TAX                    \ Copy the sixel y-coordinate to X

 JSR PlotSixelClipped   \ Plot the sixel

.out

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: CNT
\       Type: Variable
\   Category: Drawing planets
\    Summary: A counter for use in drawing Saturn's planetary body
\
\ ------------------------------------------------------------------------------
\
\ Defines the number of iterations of the PLL1 loop, which draws the planet part
\ of the loading screen's Saturn.
\
\ ******************************************************************************

.CNT

 EQUW &0300             \ The number of iterations of the PLL1 loop (768)

\ ******************************************************************************
\
\       Name: CNT2
\       Type: Variable
\   Category: Drawing planets
\    Summary: A counter for use in drawing Saturn's background stars
\
\ ------------------------------------------------------------------------------
\
\ Defines the number of iterations of the PLL2 loop, which draws the background
\ stars on the loading screen.
\
\ ******************************************************************************

.CNT2

 EQUW &01DD             \ The number of iterations of the PLL2 loop (477)

\ ******************************************************************************
\
\       Name: CNT3
\       Type: Variable
\   Category: Drawing planets
\    Summary: A counter for use in drawing Saturn's rings
\
\ ------------------------------------------------------------------------------
\
\ Defines the number of iterations of the PLL3 loop, which draws the rings
\ around the loading screen's Saturn.
\
\ ******************************************************************************

.CNT3

 EQUW &0333             \ The number of iterations of the PLL3 loop (819)

\ ******************************************************************************
\
\       Name: ROOT
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate ZP = SQRT(ZP(1 0))
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following square root:
\
\   ZP = SQRT(ZP(1 0))
\
\ This routine is identical to LL5 in the main game code - it even has the same
\ label names. The only difference is that LL5 calculates Q = SQRT(R Q), but
\ apart from the variables used, the instructions are identical, so see the LL5
\ routine in the main game code for more details on the algorithm used here.
\
\ ******************************************************************************

.ROOT

 LDY ZP+1               \ Set (Y Q) = ZP(1 0)
 LDA ZP
 STA Q

                        \ So now to calculate ZP = SQRT(Y Q)

 LDX #0                 \ Set X = 0, to hold the remainder

 STX ZP                 \ Set ZP = 0, to hold the result

 LDA #8                 \ Set P = 8, to use as a loop counter
 STA P

.LL6

 CPX ZP                 \ If X < ZP, jump to LL7
 BCC LL7

 BNE LL8                \ If X > ZP, jump to LL8

 CPY #64                \ If Y < 64, jump to LL7 with the C flag clear,
 BCC LL7                \ otherwise fall through into LL8 with the C flag set

.LL8

 TYA                    \ Set Y = Y - 64
 SBC #64                \
 TAY                    \ This subtraction will work as we know C is set from
                        \ the BCC above, and the result will not underflow as we
                        \ already checked that Y >= 64, so the C flag is also
                        \ set for the next subtraction

 TXA                    \ Set X = X - ZP
 SBC ZP
 TAX

.LL7

 ROL ZP                 \ Shift the result in Q to the left, shifting the C flag
                        \ into bit 0 and bit 7 into the C flag

 ASL Q                  \ Shift the dividend in (Y S) to the left, inserting
 TYA                    \ bit 7 from above into bit 0
 ROL A
 TAY

 TXA                    \ Shift the remainder in X to the left
 ROL A
 TAX

 ASL Q                  \ Shift the dividend in (Y S) to the left
 TYA
 ROL A
 TAY

 TXA                    \ Shift the remainder in X to the left
 ROL A
 TAX

 DEC P                  \ Decrement the loop counter

 BNE LL6                \ Loop back to LL6 until we have done 8 loops

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: OSB
\       Type: Subroutine
\   Category: Utility routines
\    Summary: A convenience routine for calling OSBYTE with Y = 0
\
\ ******************************************************************************

.OSB

 LDY #0                 \ Call OSBYTE with Y = 0, returning from the subroutine
 JMP OSBYTE             \ using a tail call (so we can call OSB to call OSBYTE
                        \ for when we know we want Y set to 0)

 MODE7_HIGH_Y = 3*25    \ The last sixel y-coordinate we can draw sixels in + 1
                        \ (so we can draw on the whole screen)

 INCLUDE "1-source-files/main-sources/elite-teletext-sixels.asm"

\ ******************************************************************************
\
\       Name: DrawSaturn
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Insert a graphics control character on rows 2 onwards
\
\ ******************************************************************************

.DrawSaturn

 LDA #132               \ Style the top and bottom rows as yellow text on a blue
 STA MODE7_VRAM         \ background
 STA MODE7_VRAM+(23*&28)
 LDA #157
 STA MODE7_VRAM+1
 STA MODE7_VRAM+(23*&28)+1
 LDA #131
 STA MODE7_VRAM+2
 STA MODE7_VRAM+(23*&28)+2

 LDA #151               \ White graphics

 FOR n, 1, 22
 
  STA MODE7_VRAM + n*40 \ Set rows 1 to 22 to white graphics
 
 NEXT

 LDA #LO(MODE7_VRAM+MODE7_INDENT+5)      \ Print the title text
 STA ZP
 LDA #HI(MODE7_VRAM+MODE7_INDENT+5)
 STA ZP+1
 LDA #LO(text1)
 STA P
 LDA #HI(text1)
 STA P+1
 JSR PrintZeroString

 LDA #LO(MODE7_VRAM+(23*&28)+MODE7_INDENT+5)   \ Print the subtitle text
 STA ZP
 LDA #HI(MODE7_VRAM+(23*&28)+MODE7_INDENT+5)
 STA ZP+1
 LDA #LO(text2)
 STA P
 LDA #HI(text2)
 STA P+1
 JSR PrintZeroString

 JMP PLL1               \ Draw Saturn, returning from the subroutine using a
                        \ tail call

\ ******************************************************************************
\
\       Name: PrintZeroString
\       Type: Subroutine
\   Category: Teletext Elite
\    Summary: Print a null-terminated string
\
\ ******************************************************************************

.PrintZeroString

 LDY #0                 \ Sey Y = 0 to act as an index into the string

.pzer1

 LDA (P),Y              \ Copy the Y-th byte of the message from P(1 0)

 BEQ pzer2              \ If it is zero, jump to pzer2 to return from the
                        \ subroutine

 STA (ZP),Y             \ Poke the byte into screen memory in ZP(1 0)

 INY                    \ Increment the index

 BPL pzer1              \ Loop back to print the next character

.pzer2

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: text1
\       Type: Variable
\   Category: Teletext Elite
\    Summary: Loading screen title
\
\ ******************************************************************************

.text1

 EQUS "Loading Elite over Econet"
 EQUB 0

\ ******************************************************************************
\
\       Name: text2
\       Type: Variable
\   Category: Teletext Elite
\    Summary: Loading screen sub title
\
\ ******************************************************************************

.text2

 EQUS "On the TNMoC Econet Cloud"
 EQUB 0

\ ******************************************************************************
\
\ Save MENU.bin
\
\ ******************************************************************************

 PRINT "S.MENU ", ~CODE%, " ", ~P%, " ", ~LOAD%, " ", ~LOAD%
 SAVE "3-assembled-output/MENU.bin", CODE%, P%, LOAD%
