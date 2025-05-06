\ ******************************************************************************
\
\ ELITE OVER ECONET SERVER MENU SOURCE (Part 2)
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
\   * MENU2.bin
\
\ ******************************************************************************

 INCLUDE "1-source-files/main-sources/elite-build-options.asm"

 _IB_DISC               = (_VARIANT = 1)
 _STH_DISC              = (_VARIANT = 2)
 _SRAM_DISC             = (_VARIANT = 3)

 GUARD &7C00            \ Guard against assembling over mode 7 screen memory

 _DOCKED = TRUE         \ Set compilation flag for docked vs flight code

 _LOADER = FALSE        \ Set compilation flag for loader

 INCLUDE "1-source-files/main-sources/elite-teletext-macros.asm"

\ ******************************************************************************
\
\ Configuration variables
\
\ ******************************************************************************

 CODE% = &2400          \ The address where the code will be run

 LOAD% = &2400          \ The address where the code will be loaded

 NOSH = 1               \ The maximum number of ships in our local bubble of
                        \ universe

 NTY = 31               \ The number of different ship types

 MSL = 1                \ Ship type for a missile

 SST = 2                \ Ship type for a Coriolis space station

 ESC = 3                \ Ship type for an escape pod

 PLT = 4                \ Ship type for an alloy plate

 OIL = 5                \ Ship type for a cargo canister

 AST = 7                \ Ship type for an asteroid

 SPL = 8                \ Ship type for a splinter

 SHU = 9                \ Ship type for a Shuttle

 CYL = 11               \ Ship type for a Cobra Mk III

 ANA = 14               \ Ship type for an Anaconda

 COPS = 16              \ Ship type for a Viper

 SH3 = 17               \ Ship type for a Sidewinder

 KRA = 19               \ Ship type for a Krait

 ADA = 20               \ Ship type for an Adder

 WRM = 23               \ Ship type for a Worm

 CYL2 = 24              \ Ship type for a Cobra Mk III (pirate)

 ASP = 25               \ Ship type for an Asp Mk II

 THG = 29               \ Ship type for a Thargoid

 TGL = 30               \ Ship type for a Thargon

 CON = 31               \ Ship type for a Constrictor

 NI% = 37               \ The number of bytes in each ship's data block (as
                        \ stored in INWK and K%)

 X = 128                \ The centre x-coordinate of the 256 x 192 space view

 Y = 96                 \ The centre y-coordinate of the 256 x 192 space view

 VIA = &FE00            \ Memory-mapped space for accessing internal hardware,
                        \ such as the video ULA, 6845 CRTC and 6522 VIAs (also
                        \ known as SHEILA)

 OSBYTE = &FFF4         \ The address for the OSBYTE routine

 OSWRCH = &FFEE         \ The address for the OSWRCH routine

 OSCLI = &FFF7          \ The address for the OSCLI routine

\ ******************************************************************************
\
\       Name: ZP
\       Type: Workspace
\    Address: &0000 to &00B0
\   Category: Workspaces
\    Summary: Lots of important variables are stored in the zero page workspace
\             as it is quicker and more space-efficient to access memory here
\
\ ******************************************************************************

 ORG &0070

.ZP

 SKIP 0                 \ The start of the zero page workspace

.XX1

 SKIP 0                 \ This is an alias for INWK that is used in the main
                        \ ship-drawing routine at LL9

.INWK

 SKIP 33                \ The zero-page internal workspace for the current ship
                        \ data block
                        \
                        \ As operations on zero page locations are faster and
                        \ have smaller opcodes than operations on the rest of
                        \ the addressable memory, Elite tends to store oft-used
                        \ data here. A lot of the routines in Elite need to
                        \ access and manipulate ship data, so to make this an
                        \ efficient exercise, the ship data is first copied from
                        \ the ship data blocks at K% into INWK (or, when new
                        \ ships are spawned, from the blueprints at XX21). See
                        \ the deep dive on "Ship data blocks" for details of
                        \ what each of the bytes in the INWK data block
                        \ represents

.XX19

 SKIP NI% - 34          \ XX19(1 0) shares its location with INWK(34 33), which
                        \ contains the address of the ship line heap

.NEWB

 SKIP 1                 \ The ship's "new byte flags" (or NEWB flags)
                        \
                        \ Contains details about the ship's type and associated
                        \ behaviour, such as whether they are a trader, a bounty
                        \ hunter, a pirate, currently hostile, in the process of
                        \ docking, inside the hold having been scooped, and so
                        \ on. The default values for each ship type are taken
                        \ from the table at E%, and you can find out more detail
                        \ in the deep dive on "Advanced tactics with the NEWB
                        \ flags"

 ORG &00B0

.P

 SKIP 3                 \ Temporary storage, used in a number of places

.XX0

 SKIP 2                 \ Temporary storage, used to store the address of a ship
                        \ blueprint. For example, it is used when we add a new
                        \ ship to the local bubble in routine NWSHP, and it
                        \ contains the address of the current ship's blueprint
                        \ as we loop through all the nearby ships in the main
                        \ flight loop

.INF

 SKIP 2                 \ Temporary storage, typically used for storing the
                        \ address of a ship's data block, so it can be copied
                        \ to and from the internal workspace at INWK

.V

 SKIP 2                 \ Temporary storage, typically used for storing an
                        \ address pointer

.U

 SKIP 1                 \ Temporary storage, used in a number of places

.Q

 SKIP 1                 \ Temporary storage, used in a number of places

.R

 SKIP 1                 \ Temporary storage, used in a number of places

.S

 SKIP 1                 \ Temporary storage, used in a number of places

.T

 SKIP 1                 \ Temporary storage, used in a number of places

 PRINT "Zero page variables from ", ~ZP, " to ", ~P%

\ ******************************************************************************
\
\       Name: XX3
\       Type: Workspace
\    Address: &0100 to the top of the descending stack
\   Category: Workspaces
\    Summary: Temporary storage space for complex calculations
\
\ ------------------------------------------------------------------------------
\
\ Used as heap space for storing temporary data during calculations. Shared with
\ the descending 6502 stack, which works down from &01FF.
\
\ ******************************************************************************

 ORG &0100

.XX3

 SKIP 256               \ Temporary storage, typically used for storing tables
                        \ of values such as screen coordinates or ship data

\ ******************************************************************************
\
\ ELITE A FILE
\
\ ******************************************************************************

 ORG CODE%

\ ******************************************************************************
\
\       Name: S%
\       Type: Workspace
\   Category: Workspaces
\    Summary: Entry points and vector addresses in the main docked code
\
\ ******************************************************************************

.S%

 JMP BEGIN              \ Decrypt the main docked code and start a new game

\ ******************************************************************************
\
\       Name: K%
\       Type: Workspace
\   Category: Workspaces
\    Summary: Ship data blocks and ship line heaps
\  Deep dive: Ship data blocks
\             The local bubble of universe
\
\ ------------------------------------------------------------------------------
\
\ Contains ship data for all the ships, planets, suns and space stations in our
\ local bubble of universe, along with their corresponding ship line heaps.
\
\ The blocks are pointed to by the lookup table at location UNIV. The first 444
\ bytes of the K% workspace hold ship data on up to 12 ships, with 37 (NI%)
\ bytes per ship, and the ship line heap grows downwards from WP at the end of
\ the K% workspace.
\
\ See the deep dive on "Ship data blocks" for details on ship data blocks, and
\ the deep dive on "The local bubble of universe" for details of how Elite
\ stores the local universe in K%, FRIN and UNIV.
\
\ ******************************************************************************

.K%

 SKIP NOSH * NI%        \ Ship data blocks and ship line heap

 SKIP 255               \ Ship line heap

.LS%

 SKIP 1

\ ******************************************************************************
\
\       Name: UP
\       Type: Workspace
\   Category: Workspaces
\    Summary: Ship slots, variables
\
\ ******************************************************************************

.UP

.FRIN

 SKIP NOSH + 1          \ Slots for the ships in the local bubble of universe
                        \
                        \ There are #NOSH + 1 slots, but the ship-spawning
                        \ routine at NWSHP only populates #NOSH of them, so
                        \ there are 13 slots but only 12 are used for ships
                        \ (the last slot is effectively used as a null
                        \ terminator when shuffling the slots down in the
                        \ KILLSHP routine)
                        \
                        \ See the deep dive on "The local bubble of universe"
                        \ for details of how Elite stores the local universe in
                        \ FRIN, UNIV and K%

.SLSP

 SKIP 2                 \ The address of the bottom of the ship line heap
                        \
                        \ The ship line heap is a descending block of memory
                        \ that starts at WP and descends down to SLSP. It can be
                        \ extended downwards by the NWSHP routine when adding
                        \ new ships (and their associated ship line heaps), in
                        \ which case SLSP is lowered to provide more heap space,
                        \ assuming there is enough free memory to do so

.XX16

 SKIP 18                \ Temporary storage for a block of values, used in a
                        \ number of places

.T1

 SKIP 1                 \ Temporary storage, used in a number of places

.XX

 SKIP 2                 \ Temporary storage, typically used for storing a 16-bit
                        \ x-coordinate

.BETA

 SKIP 1                 \ The current pitch angle beta, which is reduced from
                        \ JSTY to a sign-magnitude value between -8 and +8
                        \
                        \ This describes how fast we are pitching our ship, and
                        \ determines how fast the universe pitches around us

.ALPHA

 SKIP 1                 \ The current roll angle alpha, which is reduced from
                        \ JSTX to a sign-magnitude value between -31 and +31
                        \
                        \ This describes how fast we are rolling our ship, and
                        \ determines how fast the universe rolls around us

.XX15

 SKIP 0                 \ Temporary storage, typically used for storing screen
                        \ coordinates in line-drawing routines
                        \
                        \ There are six bytes of storage, from XX15 TO XX15+5.
                        \ The first four bytes have the following aliases:
                        \
                        \   X1 = XX15
                        \   Y1 = XX15+1
                        \   X2 = XX15+2
                        \   Y2 = XX15+3
                        \
                        \ These are typically used for describing lines in terms
                        \ of screen coordinates, i.e. (X1, Y1) to (X2, Y2)
                        \
                        \ The last two bytes of XX15 do not have aliases

.X1

 SKIP 1                 \ Temporary storage, typically used for x-coordinates in
                        \ line-drawing routines

.Y1

 SKIP 1                 \ Temporary storage, typically used for y-coordinates in
                        \ line-drawing routines

.X2

 SKIP 1                 \ Temporary storage, typically used for x-coordinates in
                        \ line-drawing routines

.Y2

 SKIP 1                 \ Temporary storage, typically used for y-coordinates in
                        \ line-drawing routines

 SKIP 2                 \ The last two bytes of the XX15 block

.XX12

 SKIP 6                 \ Temporary storage for a block of values, used in a
                        \ number of places

.K

 SKIP 4                 \ Temporary storage, used in a number of places


.XX18

 SKIP 9                 \ Temporary storage used to store coordinates in the
                        \ LL9 ship-drawing routine

.YSAV

 SKIP 1                 \ Temporary storage for saving the value of the Y
                        \ register, used in a number of places

.XX17

 SKIP 1                 \ Temporary storage, used in BPRNT to store the number
                        \ of characters to print, and as the edge counter in the
                        \ main ship-drawing routine

.XX13

 SKIP 1                 \ Temporary storage, typically used in the line-drawing
                        \ routines

.MCNT

 SKIP 1                 \ The main loop counter
                        \
                        \ This counter determines how often certain actions are
                        \ performed within the main loop. See the deep dive on
                        \ "Scheduling tasks with the main loop counter" for more
                        \ details

.TYPE

 SKIP 1                 \ The current ship type
                        \
                        \ This is where we store the current ship type for when
                        \ we are iterating through the ships in the local bubble
                        \ as part of the main flight loop. See the table at XX21
                        \ for information about ship types

.SWAP

 SKIP 1                 \ Temporary storage, used to store a flag that records
                        \ whether or not we had to swap a line's start and end
                        \ coordinates around when clipping the line in routine
                        \ LL145 (the flag is used in places like BLINE to swap
                        \ them back)

.CNT

 SKIP 1                 \ Temporary storage, typically used for storing the
                        \ number of iterations required when looping

.XX4

 SKIP 1                 \ Temporary storage, used in a number of places

.XX20

 SKIP 1                 \ Temporary storage, used in a number of places

.LSNUM

 SKIP 1                 \ The pointer to the current position in the ship line
                        \ heap as we work our way through the new ship's edges
                        \ (and the corresponding old ship's edges) when drawing
                        \ the ship in the main ship-drawing routine at LL9

.LSNUM2

 SKIP 1                 \ The size of the existing ship line heap for the ship
                        \ we are drawing in LL9, i.e. the number of lines in the
                        \ old ship that is currently shown on-screen and which
                        \ we need to erase

.RAT2

 SKIP 1                 \ Temporary storage, used to store the pitch and roll
                        \ signs when moving objects and stardust

.XX2

 SKIP 14                \ Temporary storage, used to store the visibility of the
                        \ ship's faces during the ship-drawing routine at LL9

\ ******************************************************************************
\
\       Name: MVEIT (Part 1 of 9)
\       Type: Subroutine
\   Category: Moving
\    Summary: Move current ship: Tidy the orientation vectors
\  Deep dive: Program flow of the ship-moving routine
\             Scheduling tasks with the main loop counter
\
\ ------------------------------------------------------------------------------
\
\ This routine has multiple stages. This stage does the following:
\
\   * Tidy the orientation vectors for one of the ship slots
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   INWK                The current ship/planet/sun's data block
\
\   XSAV                The slot number of the current ship/planet/sun
\
\   TYPE                The type of the current ship/planet/sun
\
\ ******************************************************************************

.MVEIT

\LDA INWK+31            \ If bit 5 of ship byte #31 is set, jump to MV3 as the
\AND #%00100000         \ ship is exploding, so we don't need to tidy its
\BNE MV3                \ orientation vectors

 LDA MCNT               \ Fetch the main loop counter

\EOR XSAV               \ Fetch the slot number of the ship we are moving, EOR
 AND #15                \ with the loop counter and apply mod 15 to the result.
 BNE MV3                \ The result will be zero when "counter mod 15" matches
                        \ the slot number, so this makes sure we call TIDY 12
                        \ times every 16 main loop iterations, like this:
                        \
                        \   Iteration 0, tidy the ship in slot 0
                        \   Iteration 1, tidy the ship in slot 1
                        \   Iteration 2, tidy the ship in slot 2
                        \     ...
                        \   Iteration 11, tidy the ship in slot 11
                        \   Iteration 12, do nothing
                        \   Iteration 13, do nothing
                        \   Iteration 14, do nothing
                        \   Iteration 15, do nothing
                        \   Iteration 16, tidy the ship in slot 0
                        \     ...
                        \
                        \ and so on

 JSR TIDY               \ Call TIDY to tidy up the orientation vectors, to
                        \ prevent the ship from getting elongated and out of
                        \ shape due to the imprecise nature of trigonometry
                        \ in assembly language

.MV3

                        \ Fall through into part 7 (parts 2-6 are not required
                        \ when we are docked)

\ ******************************************************************************
\
\       Name: MVEIT (Part 7 of 9)
\       Type: Subroutine
\   Category: Moving
\    Summary: Move current ship: Rotate ship's orientation vectors by pitch/roll
\  Deep dive: Orientation vectors
\             Pitching and rolling
\
\ ------------------------------------------------------------------------------
\
\ This routine has multiple stages. This stage does the following:
\
\   * Rotate the ship's orientation vectors according to our pitch and roll
\
\ As with the previous step, this is all about moving the other ships rather
\ than us (even though we are the one doing the moving). So we rotate the
\ current ship's orientation vectors (which defines its orientation in space),
\ by the angles we are "moving" the rest of the sky through (alpha and beta, our
\ roll and pitch), so the ship appears to us to be stationary while we rotate.
\
\ ******************************************************************************

 LDY #9                 \ Apply our pitch and roll rotations to the current
 JSR MVS4               \ ship's nosev vector

 LDY #15                \ Apply our pitch and roll rotations to the current
 JSR MVS4               \ ship's roofv vector

 LDY #21                \ Apply our pitch and roll rotations to the current
 JSR MVS4               \ ship's sidev vector

\ ******************************************************************************
\
\       Name: MVEIT (Part 8 of 9)
\       Type: Subroutine
\   Category: Moving
\    Summary: Move current ship: Rotate ship about itself by its own pitch/roll
\  Deep dive: Orientation vectors
\             Pitching and rolling by a fixed angle
\
\ ------------------------------------------------------------------------------
\
\ This routine has multiple stages. This stage does the following:
\
\   * If the ship we are processing is rolling or pitching itself, rotate it and
\     apply damping if required
\
\ ******************************************************************************

 LDA INWK+30            \ Fetch the ship's pitch counter and extract the sign
 AND #%10000000         \ into RAT2
 STA RAT2

 LDA INWK+30            \ Fetch the ship's pitch counter and extract the value
 AND #%01111111         \ without the sign bit into A

 BEQ MV8                \ If the pitch counter is 0, then jump to MV8 to skip
                        \ the following, as the ship is not pitching

 CMP #%01111111         \ If bits 0-6 are set in the pitch counter (i.e. the
                        \ ship's pitch is not damping down), then the C flag
                        \ will be set by this instruction

 SBC #0                 \ Set A = A - 0 - (1 - C), so if we are damping then we
                        \ reduce A by 1, otherwise it is unchanged

 ORA RAT2               \ Change bit 7 of A to the sign we saved in RAT2, so
                        \ the updated pitch counter in A retains its sign

 STA INWK+30            \ Store the updated pitch counter in byte #30

 LDX #15                \ Rotate (roofv_x, nosev_x) by a small angle (pitch)
 LDY #9
 JSR MVS5

 LDX #17                \ Rotate (roofv_y, nosev_y) by a small angle (pitch)
 LDY #11
 JSR MVS5

 LDX #19                \ Rotate (roofv_z, nosev_z) by a small angle (pitch)
 LDY #13
 JSR MVS5

.MV8

 LDA INWK+29            \ Fetch the ship's roll counter and extract the sign
 AND #%10000000         \ into RAT2
 STA RAT2

 LDA INWK+29            \ Fetch the ship's roll counter and extract the value
 AND #%01111111         \ without the sign bit into A

 BEQ MV5                \ If the roll counter is 0, then jump to MV5 to skip the
                        \ following, as the ship is not rolling

 CMP #%01111111         \ If bits 0-6 are set in the roll counter (i.e. the
                        \ ship's roll is not damping down), then the C flag
                        \ will be set by this instruction

 SBC #0                 \ Set A = A - 0 - (1 - C), so if we are damping then we
                        \ reduce A by 1, otherwise it is unchanged

 ORA RAT2               \ Change bit 7 of A to the sign we saved in RAT2, so
                        \ the updated roll counter in A retains its sign

 STA INWK+29            \ Store the updated pitch counter in byte #29

 LDX #15                \ Rotate (roofv_x, sidev_x) by a small angle (roll)
 LDY #21
 JSR MVS5

 LDX #17                \ Rotate (roofv_y, sidev_y) by a small angle (roll)
 LDY #23
 JSR MVS5

 LDX #19                \ Rotate (roofv_z, sidev_z) by a small angle (roll)
 LDY #25
 JSR MVS5

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MVEIT (Part 9 of 9)
\       Type: Subroutine
\   Category: Moving
\    Summary: Move current ship: Redraw on scanner, if it hasn't been destroyed
\
\ ------------------------------------------------------------------------------
\
\ This routine has multiple stages. This stage does the following:
\
\   * If the ship is exploding or being removed, hide it on the scanner
\
\   * Otherwise redraw the ship on the scanner, now that it's been moved
\
\ ******************************************************************************

.MV5

 LDA INWK+31            \ Fetch the ship's exploding/killed state from byte #31

 AND #%00100000         \ If we are exploding then jump to MVD1 to remove it
 BNE MVD1               \ from the scanner permanently

 LDA INWK+31            \ Set bit 4 to keep the ship visible on the scanner
 ORA #%00010000
 STA INWK+31

.MVD1

 LDA INWK+31            \ Clear bit 4 to hide the ship on the scanner
 AND #%11101111
 STA INWK+31

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MVT1
\       Type: Subroutine
\   Category: Moving
\    Summary: Calculate (x_sign x_hi x_lo) = (x_sign x_hi x_lo) + (A R)
\
\ ------------------------------------------------------------------------------
\
\ Add the signed delta (A R) to a ship's coordinate, along the axis given in X.
\ Mathematically speaking, this routine translates the ship along a single axis
\ by a signed delta. Taking the example of X = 0, the x-axis, it does the
\ following:
\
\   (x_sign x_hi x_lo) = (x_sign x_hi x_lo) + (A R)
\
\ (In practice, MVT1 is only ever called directly with A = 0 or 128, otherwise
\ it is always called via MVT-2, which clears A apart from the sign bit. The
\ routine is written to cope with a non-zero delta_hi, so it supports a full
\ 16-bit delta, but it appears that delta_hi is only ever used to hold the
\ sign of the delta.)
\
\ The comments below assume we are adding delta to the x-axis, though the axis
\ is determined by the value of X.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   (A R)               The signed delta, so A = delta_hi and R = delta_lo
\
\   X                   Determines which coordinate axis of INWK to change:
\
\                         * X = 0 adds the delta to (x_lo, x_hi, x_sign)
\
\                         * X = 3 adds the delta to (y_lo, y_hi, y_sign)
\
\                         * X = 6 adds the delta to (z_lo, z_hi, z_sign)
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   MVT1-2              Clear bits 0-6 of A before entering MVT1
\
\ ******************************************************************************

 AND #%10000000         \ Clear bits 0-6 of A

.MVT1

 ASL A                  \ Set the C flag to the sign bit of the delta, leaving
                        \ delta_hi << 1 in A

 STA S                  \ Set S = delta_hi << 1
                        \
                        \ This also clears bit 0 of S

 LDA #0                 \ Set T = just the sign bit of delta (in bit 7)
 ROR A
 STA T

 LSR S                  \ Set S = delta_hi >> 1
                        \       = |delta_hi|
                        \
                        \ This also clear the C flag, as we know that bit 0 of
                        \ S was clear before the LSR

 EOR INWK+2,X           \ If T EOR x_sign has bit 7 set, then x_sign and delta
 BMI MV10               \ have different signs, so jump to MV10

                        \ At this point, we know x_sign and delta have the same
                        \ sign, that sign is in T, and S contains |delta_hi|,
                        \ so now we want to do:
                        \
                        \   (x_sign x_hi x_lo) = (x_sign x_hi x_lo) + (S R)
                        \
                        \ and then set the sign of the result to the same sign
                        \ as x_sign and delta

 LDA R                  \ First we add the low bytes, so:
 ADC INWK,X             \
 STA INWK,X             \   x_lo = x_lo + R

 LDA S                  \ Then we add the high bytes:
 ADC INWK+1,X           \
 STA INWK+1,X           \   x_hi = x_hi + S

 LDA INWK+2,X           \ And finally we add any carry into x_sign, and if the
 ADC #0                 \ sign of x_sign and delta in T is negative, make sure
 ORA T                  \ the result is negative (by OR'ing with T)
 STA INWK+2,X

 RTS                    \ Return from the subroutine

.MV10

                        \ If we get here, we know x_sign and delta have
                        \ different signs, with delta's sign in T, and
                        \ |delta_hi| in S, so now we want to do:
                        \
                        \   (x_sign x_hi x_lo) = (x_sign x_hi x_lo) - (S R)
                        \
                        \ and then set the sign of the result according to
                        \ the signs of x_sign and delta

 LDA INWK,X             \ First we subtract the low bytes, so:
 SEC                    \
 SBC R                  \   x_lo = x_lo - R
 STA INWK,X

 LDA INWK+1,X           \ Then we subtract the high bytes:
 SBC S                  \
 STA INWK+1,X           \   x_hi = x_hi - S

 LDA INWK+2,X           \ And finally we subtract any borrow from bits 0-6 of
 AND #%01111111         \ x_sign, and give the result the opposite sign bit to T
 SBC #0                 \ (i.e. give it the sign of the original x_sign)
 ORA #%10000000
 EOR T
 STA INWK+2,X

 BCS MV11               \ If the C flag is set by the above SBC, then our sum
                        \ above didn't underflow and is correct - to put it
                        \ another way, (x_sign x_hi x_lo) >= (S R) so the result
                        \ should indeed have the same sign as x_sign, so jump to
                        \ MV11 to return from the subroutine

                        \ Otherwise our subtraction underflowed because
                        \ (x_sign x_hi x_lo) < (S R), so we now need to flip the
                        \ subtraction around by using two's complement to this:
                        \
                        \   (S R) - (x_sign x_hi x_lo)
                        \
                        \ and then we need to give the result the same sign as
                        \ (S R), the delta, as that's the dominant figure in the
                        \ sum

 LDA #1                 \ First we subtract the low bytes, so:
 SBC INWK,X             \
 STA INWK,X             \   x_lo = 1 - x_lo

 LDA #0                 \ Then we subtract the high bytes:
 SBC INWK+1,X           \
 STA INWK+1,X           \   x_hi = 0 - x_hi

 LDA #0                 \ And then we subtract the sign bytes:
 SBC INWK+2,X           \
                        \   x_sign = 0 - x_sign

 AND #%01111111         \ Finally, we set the sign bit to the sign in T, the
 ORA T                  \ sign of the original delta, as the delta is the
 STA INWK+2,X           \ dominant figure in the sum

.MV11

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MVT3
\       Type: Subroutine
\   Category: Moving
\    Summary: Calculate K(3 2 1) = (x_sign x_hi x_lo) + K(3 2 1)
\
\ ------------------------------------------------------------------------------
\
\ Add an INWK position coordinate - i.e. x, y or z - to K(3 2 1), like this:
\
\   K(3 2 1) = (x_sign x_hi x_lo) + K(3 2 1)
\
\ The INWK coordinate to add to K(3 2 1) is specified by X.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   The coordinate to add to K(3 2 1), as follows:
\
\                         * If X = 0, add (x_sign x_hi x_lo)
\
\                         * If X = 3, add (y_sign y_hi y_lo)
\
\                         * If X = 6, add (z_sign z_hi z_lo)
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   A                   Contains a copy of the high byte of the result, K+3
\
\   X                   X is preserved
\
\ ******************************************************************************

.MVT3

 LDA K+3                \ Set S = K+3
 STA S

 AND #%10000000         \ Set T = sign bit of K(3 2 1)
 STA T

 EOR INWK+2,X           \ If x_sign has a different sign to K(3 2 1), jump to
 BMI MV13               \ MV13 to process the addition as a subtraction

 LDA K+1                \ Set K(3 2 1) = K(3 2 1) + (x_sign x_hi x_lo)
 CLC                    \ starting with the low bytes
 ADC INWK,X
 STA K+1

 LDA K+2                \ Then the middle bytes
 ADC INWK+1,X
 STA K+2

 LDA K+3                \ And finally the high bytes
 ADC INWK+2,X

 AND #%01111111         \ Setting the sign bit of K+3 to T, the original sign
 ORA T                  \ of K(3 2 1)
 STA K+3

 RTS                    \ Return from the subroutine

.MV13

 LDA S                  \ Set S = |K+3| (i.e. K+3 with the sign bit cleared)
 AND #%01111111
 STA S

 LDA INWK,X             \ Set K(3 2 1) = (x_sign x_hi x_lo) - K(3 2 1)
 SEC                    \ starting with the low bytes
 SBC K+1
 STA K+1

 LDA INWK+1,X           \ Then the middle bytes
 SBC K+2
 STA K+2

 LDA INWK+2,X           \ And finally the high bytes, doing A = |x_sign| - |K+3|
 AND #%01111111         \ and setting the C flag for testing below
 SBC S

 ORA #%10000000         \ Set the sign bit of K+3 to the opposite sign of T,
 EOR T                  \ i.e. the opposite sign to the original K(3 2 1)
 STA K+3

 BCS MV14               \ If the C flag is set, i.e. |x_sign| >= |K+3|, then
                        \ the sign of K(3 2 1). In this case, we want the
                        \ result to have the same sign as the largest argument,
                        \ which is (x_sign x_hi x_lo), which we know has the
                        \ opposite sign to K(3 2 1), and that's what we just set
                        \ the sign of K(3 2 1) to... so we can jump to MV14 to
                        \ return from the subroutine

 LDA #1                 \ We need to swap the sign of the result in K(3 2 1),
 SBC K+1                \ which we do by calculating 0 - K(3 2 1), which we can
 STA K+1                \ do with 1 - C - K(3 2 1), as we know the C flag is
                        \ clear. We start with the low bytes

 LDA #0                 \ Then the middle bytes
 SBC K+2
 STA K+2

 LDA #0                 \ And finally the high bytes
 SBC K+3

 AND #%01111111         \ Set the sign bit of K+3 to the same sign as T,
 ORA T                  \ i.e. the same sign as the original K(3 2 1), as
 STA K+3                \ that's the largest argument

.MV14

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MVS4
\       Type: Subroutine
\   Category: Moving
\    Summary: Apply pitch and roll to an orientation vector
\  Deep dive: Orientation vectors
\             Pitching and rolling
\
\ ------------------------------------------------------------------------------
\
\ Apply pitch and roll angles alpha and beta to the orientation vector in Y.
\
\ Specifically, this routine rotates a point (x, y, z) around the origin by
\ pitch alpha and roll beta, using the small angle approximation to make the
\ maths easier, and incorporating the Minsky circle algorithm to make the
\ rotation more stable (though more elliptic).
\
\ If that paragraph makes sense to you, then you should probably be writing
\ this commentary! For the rest of us, there's a detailed explanation of all
\ this in the deep dive on "Pitching and rolling".
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   Y                   Determines which of the INWK orientation vectors to
\                       transform:
\
\                         * Y = 9 rotates nosev: (nosev_x, nosev_y, nosev_z)
\
\                         * Y = 15 rotates roofv: (roofv_x, roofv_y, roofv_z)
\
\                         * Y = 21 rotates sidev: (sidev_x, sidev_y, sidev_z)
\
\ ******************************************************************************

.MVS4

 LDA ALPHA              \ Set Q = alpha (the roll angle to rotate through)
 STA Q

 LDX INWK+2,Y           \ Set (S R) = nosev_y
 STX R
 LDX INWK+3,Y
 STX S

 LDX INWK,Y             \ These instructions have no effect as MAD overwrites
 STX P                  \ X and P when called, but they set X = P = nosev_x_lo

 LDA INWK+1,Y           \ Set A = -nosev_x_hi
 EOR #%10000000

 JSR MAD                \ Set (A X) = Q * A + (S R)
 STA INWK+3,Y           \           = alpha * -nosev_x_hi + nosev_y
 STX INWK+2,Y           \
                        \ and store (A X) in nosev_y, so this does:
                        \
                        \ nosev_y = nosev_y - alpha * nosev_x_hi

 STX P                  \ This instruction has no effect as MAD overwrites P,
                        \ but it sets P = nosev_y_lo

 LDX INWK,Y             \ Set (S R) = nosev_x
 STX R
 LDX INWK+1,Y
 STX S

 LDA INWK+3,Y           \ Set A = nosev_y_hi

 JSR MAD                \ Set (A X) = Q * A + (S R)
 STA INWK+1,Y           \           = alpha * nosev_y_hi + nosev_x
 STX INWK,Y             \
                        \ and store (A X) in nosev_x, so this does:
                        \
                        \ nosev_x = nosev_x + alpha * nosev_y_hi

 STX P                  \ This instruction has no effect as MAD overwrites P,
                        \ but it sets P = nosev_x_lo

 LDA BETA               \ Set Q = beta (the pitch angle to rotate through)
 STA Q

 LDX INWK+2,Y           \ Set (S R) = nosev_y
 STX R
 LDX INWK+3,Y
 STX S
 LDX INWK+4,Y

 STX P                  \ This instruction has no effect as MAD overwrites P,
                        \ but it sets P = nosev_y

 LDA INWK+5,Y           \ Set A = -nosev_z_hi
 EOR #%10000000

 JSR MAD                \ Set (A X) = Q * A + (S R)
 STA INWK+3,Y           \           = beta * -nosev_z_hi + nosev_y
 STX INWK+2,Y           \
                        \ and store (A X) in nosev_y, so this does:
                        \
                        \ nosev_y = nosev_y - beta * nosev_z_hi

 STX P                  \ This instruction has no effect as MAD overwrites P,
                        \ but it sets P = nosev_y_lo

 LDX INWK+4,Y           \ Set (S R) = nosev_z
 STX R
 LDX INWK+5,Y
 STX S

 LDA INWK+3,Y           \ Set A = nosev_y_hi

 JSR MAD                \ Set (A X) = Q * A + (S R)
 STA INWK+5,Y           \           = beta * nosev_y_hi + nosev_z
 STX INWK+4,Y           \
                        \ and store (A X) in nosev_z, so this does:
                        \
                        \ nosev_z = nosev_z + beta * nosev_y_hi

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MVS5
\       Type: Subroutine
\   Category: Moving
\    Summary: Apply a 3.6 degree pitch or roll to an orientation vector
\  Deep dive: Orientation vectors
\             Pitching and rolling by a fixed angle
\
\ ------------------------------------------------------------------------------
\
\ Pitch or roll a ship by a small, fixed amount (1/16 radians, or 3.6 degrees),
\ in a specified direction, by rotating the orientation vectors. The vectors to
\ rotate are given in X and Y, and the direction of the rotation is given in
\ RAT2. The calculation is as follows:
\
\   * If the direction is positive:
\
\     X = X * (1 - 1/512) + Y / 16
\     Y = Y * (1 - 1/512) - X / 16
\
\   * If the direction is negative:
\
\     X = X * (1 - 1/512) - Y / 16
\     Y = Y * (1 - 1/512) + X / 16
\
\ So if X = 15 (roofv_x), Y = 21 (sidev_x) and RAT2 is positive, it does this:
\
\   roofv_x = roofv_x * (1 - 1/512)  + sidev_x / 16
\   sidev_x = sidev_x * (1 - 1/512)  - roofv_x / 16
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   The first vector to rotate:
\
\                         * If X = 15, rotate roofv_x
\
\                         * If X = 17, rotate roofv_y
\
\                         * If X = 19, rotate roofv_z
\
\                         * If X = 21, rotate sidev_x
\
\                         * If X = 23, rotate sidev_y
\
\                         * If X = 25, rotate sidev_z
\
\   Y                   The second vector to rotate:
\
\                         * If Y = 9,  rotate nosev_x
\
\                         * If Y = 11, rotate nosev_y
\
\                         * If Y = 13, rotate nosev_z
\
\                         * If Y = 21, rotate sidev_x
\
\                         * If Y = 23, rotate sidev_y
\
\                         * If Y = 25, rotate sidev_z
\
\   RAT2                The direction of the pitch or roll to perform, positive
\                       or negative (i.e. the sign of the roll or pitch counter
\                       in bit 7)
\
\ ******************************************************************************

.MVS5

 LDA INWK+1,X           \ Fetch roofv_x_hi, clear the sign bit, divide by 2 and
 AND #%01111111         \ store in T, so:
 LSR A                  \
 STA T                  \ T = |roofv_x_hi| / 2
                        \   = |roofv_x| / 512
                        \
                        \ The above is true because:
                        \
                        \ |roofv_x| = |roofv_x_hi| * 256 + roofv_x_lo
                        \
                        \ so:
                        \
                        \ |roofv_x| / 512 = |roofv_x_hi| * 256 / 512
                        \                    + roofv_x_lo / 512
                        \                  = |roofv_x_hi| / 2

 LDA INWK,X             \ Now we do the following subtraction:
 SEC                    \
 SBC T                  \ (S R) = (roofv_x_hi roofv_x_lo) - |roofv_x| / 512
 STA R                  \       = (1 - 1/512) * roofv_x
                        \
                        \ by doing the low bytes first

 LDA INWK+1,X           \ And then the high bytes (the high byte of the right
 SBC #0                 \ side of the subtraction being 0)
 STA S

 LDA INWK,Y             \ Set P = nosev_x_lo
 STA P

 LDA INWK+1,Y           \ Fetch the sign of nosev_x_hi (bit 7) and store in T
 AND #%10000000
 STA T

 LDA INWK+1,Y           \ Fetch nosev_x_hi into A and clear the sign bit, so
 AND #%01111111         \ A = |nosev_x_hi|

 LSR A                  \ Set (A P) = (A P) / 16
 ROR P                  \           = |nosev_x_hi nosev_x_lo| / 16
 LSR A                  \           = |nosev_x| / 16
 ROR P
 LSR A
 ROR P
 LSR A
 ROR P

 ORA T                  \ Set the sign of A to the sign in T (i.e. the sign of
                        \ the original nosev_x), so now:
                        \
                        \ (A P) = nosev_x / 16

 EOR RAT2               \ Give it the sign as if we multiplied by the direction
                        \ by the pitch or roll direction

 STX Q                  \ Store the value of X so it can be restored after the
                        \ call to ADD

 JSR ADD                \ (A X) = (A P) + (S R)
                        \       = +/-nosev_x / 16 + (1 - 1/512) * roofv_x

 STA K+1                \ Set K(1 0) = (1 - 1/512) * roofv_x +/- nosev_x / 16
 STX K

 LDX Q                  \ Restore the value of X from before the call to ADD

 LDA INWK+1,Y           \ Fetch nosev_x_hi, clear the sign bit, divide by 2 and
 AND #%01111111         \ store in T, so:
 LSR A                  \
 STA T                  \ T = |nosev_x_hi| / 2
                        \   = |nosev_x| / 512

 LDA INWK,Y             \ Now we do the following subtraction:
 SEC                    \
 SBC T                  \ (S R) = (nosev_x_hi nosev_x_lo) - |nosev_x| / 512
 STA R                  \       = (1 - 1/512) * nosev_x
                        \
                        \ by doing the low bytes first

 LDA INWK+1,Y           \ And then the high bytes (the high byte of the right
 SBC #0                 \ side of the subtraction being 0)
 STA S

 LDA INWK,X             \ Set P = roofv_x_lo
 STA P

 LDA INWK+1,X           \ Fetch the sign of roofv_x_hi (bit 7) and store in T
 AND #%10000000
 STA T

 LDA INWK+1,X           \ Fetch roofv_x_hi into A and clear the sign bit, so
 AND #%01111111         \ A = |roofv_x_hi|

 LSR A                  \ Set (A P) = (A P) / 16
 ROR P                  \           = |roofv_x_hi roofv_x_lo| / 16
 LSR A                  \           = |roofv_x| / 16
 ROR P
 LSR A
 ROR P
 LSR A
 ROR P

 ORA T                  \ Set the sign of A to the opposite sign to T (i.e. the
 EOR #%10000000         \ sign of the original -roofv_x), so now:
                        \
                        \ (A P) = -roofv_x / 16

 EOR RAT2               \ Give it the sign as if we multiplied by the direction
                        \ by the pitch or roll direction

 STX Q                  \ Store the value of X so it can be restored after the
                        \ call to ADD

 JSR ADD                \ (A X) = (A P) + (S R)
                        \       = -/+roofv_x / 16 + (1 - 1/512) * nosev_x

 STA INWK+1,Y           \ Set nosev_x = (1-1/512) * nosev_x -/+ roofv_x / 16
 STX INWK,Y

 LDX Q                  \ Restore the value of X from before the call to ADD

 LDA K                  \ Set roofv_x = K(1 0)
 STA INWK,X             \              = (1-1/512) * roofv_x +/- nosev_x / 16
 LDA K+1
 STA INWK+1,X

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MVT6
\       Type: Subroutine
\   Category: Moving
\    Summary: Calculate (A P+2 P+1) = (x_sign x_hi x_lo) + (A P+2 P+1)
\
\ ------------------------------------------------------------------------------
\
\ Do the following calculation, for the coordinate given by X (so this is what
\ it does for the x-coordinate):
\
\   (A P+2 P+1) = (x_sign x_hi x_lo) + (A P+2 P+1)
\
\ A is a sign bit and is not included in the calculation, but bits 0-6 of A are
\ preserved. Bit 7 is set to the sign of the result.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   A                   The sign of P(2 1) in bit 7
\
\   P(2 1)              The 16-bit value we want to add the coordinate to
\
\   X                   The coordinate to add, as follows:
\
\                         * If X = 0, add to (x_sign x_hi x_lo)
\
\                         * If X = 3, add to (y_sign y_hi y_lo)
\
\                         * If X = 6, add to (z_sign z_hi z_lo)
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   A                   The sign of the result (in bit 7)
\
\ ******************************************************************************

.MVT6

 TAY                    \ Store argument A into Y, for later use

 EOR INWK+2,X           \ Set A = A EOR x_sign

 BMI MV50               \ If the sign is negative, i.e. A and x_sign have
                        \ different signs, jump to MV50

                        \ The signs are the same, so we can add the two
                        \ arguments and keep the sign to get the result

 LDA P+1                \ First we add the low bytes:
 CLC                    \
 ADC INWK,X             \   P+1 = P+1 + x_lo
 STA P+1

 LDA P+2                \ And then the high bytes:
 ADC INWK+1,X           \
 STA P+2                \   P+2 = P+2 + x_hi

 TYA                    \ Restore the original A argument that we stored earlier
                        \ so that we keep the original sign

 RTS                    \ Return from the subroutine

.MV50

 LDA INWK,X             \ First we subtract the low bytes:
 SEC                    \
 SBC P+1                \   P+1 = x_lo - P+1
 STA P+1

 LDA INWK+1,X           \ And then the high bytes:
 SBC P+2                \
 STA P+2                \   P+2 = x_hi - P+2

 BCC MV51               \ If the last subtraction underflowed, then the C flag
                        \ will be clear and x_hi < P+2, so jump to MV51 to
                        \ negate the result

 TYA                    \ Restore the original A argument that we stored earlier
 EOR #%10000000         \ but flip bit 7, which flips the sign. We do this
                        \ because x_hi >= P+2 so we want the result to have the
                        \ same sign as x_hi (as it's the dominant side in this
                        \ calculation). The sign of x_hi is x_sign, and x_sign
                        \ has the opposite sign to A, so we flip the sign in A
                        \ to return the correct result

 RTS                    \ Return from the subroutine

.MV51

 LDA #1                 \ Our subtraction underflowed, so we negate the result
 SBC P+1                \ using two's complement, first with the low byte:
 STA P+1                \
                        \   P+1 = 1 - P+1

 LDA #0                 \ And then the high byte:
 SBC P+2                \
 STA P+2                \   P+2 = 0 - P+2

 TYA                    \ Restore the original A argument that we stored earlier
                        \ as this is the correct sign for the result. This is
                        \ because x_hi < P+2, so we want to return the same sign
                        \ as P+2, the dominant side

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: UNIV
\       Type: Variable
\   Category: Universe
\    Summary: Table of pointers to the local universe's ship data blocks
\  Deep dive: The local bubble of universe
\
\ ------------------------------------------------------------------------------
\
\ See the deep dive on "Ship data blocks" for details on ship data blocks, and
\ the deep dive on "The local bubble of universe" for details of how Elite
\ stores the local universe in K%, FRIN and UNIV.
\
\ ******************************************************************************

.UNIV

 FOR I%, 0, NOSH

  EQUW K% + I% * NI%    \ Address of block no. I%, of size NI%, in workspace K%

 NEXT


\ ******************************************************************************
\
\       Name: LOIN (Part 1 of 7)
\       Type: Subroutine
\   Category: Drawing lines
\    Summary: Draw a line: Calculate the line gradient in the form of deltas
\  Deep dive: Bresenham's line algorithm
\
\ ------------------------------------------------------------------------------
\
\ This routine draws a line from (X1, Y1) to (X2, Y2). It has multiple stages.
\ This stage calculates the line deltas.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X1                  The screen x-coordinate of the start of the line
\
\   Y1                  The screen y-coordinate of the start of the line
\
\   X2                  The screen x-coordinate of the end of the line
\
\   Y2                  The screen y-coordinate of the end of the line
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   Y                   Y is preserved
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   LL30                LL30 is a synonym for LOIN and draws a line from
\                       (X1, Y1) to (X2, Y2)
\
\ ******************************************************************************

.LL30

 SKIP 0                 \ LL30 is a synonym for LOIN
                        \
                        \ In the cassette and disc versions of Elite, LL30 and
                        \ LOIN are synonyms for the same routine, presumably
                        \ because the two developers each had their own line
                        \ routines to start with, and then chose one of them for
                        \ the final game

.LOIN

 STY YSAV               \ Store Y into YSAV, so we can preserve it across the
                        \ call to this subroutine

 LDA #0                 \ Set SWAP to 0 as we don't need to swap coordinates
 STA SWAP

 LDA X1                 \ Scale the pixel x-coordinate in A into sixels
 SCALE_SIXEL_X
 TAX

 LDA Y1                 \ Scale the pixel y-coordinate in A into sixels
 SCALE_SIXEL_Y
 TAY

 JSR MoveToSixel        \ Move the graphics cursor to the start of the line at
                        \ (X1, Y1)

 LDA X2                 \ Scale the pixel x-coordinate in A into sixels
 SCALE_SIXEL_X
 TAX

 LDA Y2                 \ Scale the pixel y-coordinate in A into sixels
 SCALE_SIXEL_Y
 TAY

 JSR DrawToSixel        \ Draw a line from (X1, Y1) to (X2, Y2)

 LDY YSAV               \ Restore Y from YSAV, so that it's preserved

.HL6

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: HLOIN
\       Type: Subroutine
\   Category: Drawing lines
\    Summary: Draw a horizontal line from (X1, Y1) to (X2, Y1)
\  Deep dive: Drawing monochrome pixels in mode 4
\
\ ------------------------------------------------------------------------------
\
\ We do not draw a pixel at the right end of the line.
\
\ To understand how this routine works, you might find it helpful to read the
\ deep dive on "Drawing monochrome pixels in mode 4".
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   Y                   Y is preserved
\
\ ******************************************************************************

.HLOIN

 STY YSAV               \ Store Y into YSAV, so we can preserve it across the
                        \ call to this subroutine

 LDX X1                 \ Set X = X1

 CPX X2                 \ If X1 = X2 then the start and end points are the same,
 BEQ HL6                \ so return from the subroutine (as HL6 contains an RTS)

 BCC HL5                \ If X1 < X2, jump to HL5 to skip the following code, as
                        \ (X1, Y1) is already the left point

 LDA X2                 \ Swap the values of X1 and X2, so we know that (X1, Y1)
 STA X1                 \ is on the left and (X2, Y1) is on the right
 STX X2

 TAX                    \ Set X = X1

.HL5

 LDA Y1                 \ Draw a line from from (X1, Y1) to (X2, Y1)
 STA Y2
 JSR LOIN

 LDY YSAV               \ Restore Y from YSAV, so that it's preserved

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: SQUA
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Clear bit 7 of A and calculate (A P) = A * A
\
\ ------------------------------------------------------------------------------
\
\ Do the following multiplication of unsigned 8-bit numbers, after first
\ clearing bit 7 of A:
\
\   (A P) = A * A
\
\ ******************************************************************************

.SQUA

 AND #%01111111         \ Clear bit 7 of A and fall through into SQUA2 to set
                        \ (A P) = A * A

\ ******************************************************************************
\
\       Name: SQUA2
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A P) = A * A
\
\ ------------------------------------------------------------------------------
\
\ Do the following multiplication of unsigned 8-bit numbers:
\
\   (A P) = A * A
\
\ ******************************************************************************

.SQUA2

 STA P                  \ Copy A into P and X
 TAX

 BNE MU11               \ If X = 0 fall through into MU1 to return a 0,
                        \ otherwise jump to MU11 to return P * X

\ ******************************************************************************
\
\       Name: MU1
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Copy X into P and A, and clear the C flag
\
\ ------------------------------------------------------------------------------
\
\ Used to return a 0 result quickly from MULTU below.
\
\ ******************************************************************************

.MU1

 CLC                    \ Clear the C flag

 STX P                  \ Copy X into P and A
 TXA

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MULTU
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A P) = P * Q
\
\ ------------------------------------------------------------------------------
\
\ Do the following multiplication of unsigned 8-bit numbers:
\
\   (A P) = P * Q
\
\ ******************************************************************************

.MULTU

 LDX Q                  \ Set X = Q

 BEQ MU1                \ If X = Q = 0, jump to MU1 to copy X into P and A,
                        \ clear the C flag and return from the subroutine using
                        \ a tail call

                        \ Otherwise fall through into MU11 to set (A P) = P * X

\ ******************************************************************************
\
\       Name: MU11
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A P) = P * X
\  Deep dive: Shift-and-add multiplication
\
\ ------------------------------------------------------------------------------
\
\ Do the following multiplication of two unsigned 8-bit numbers:
\
\   (A P) = P * X
\
\ This uses the same shift-and-add approach as MULT1, but it's simpler as we
\ are dealing with unsigned numbers in P and X. See the deep dive on
\ "Shift-and-add multiplication" for a discussion of how this algorithm works.
\
\ ******************************************************************************

.MU11

 DEX                    \ Set T = X - 1
 STX T                  \
                        \ We subtract 1 as the C flag will be set when we want
                        \ to do an addition in the loop below

 LDA #0                 \ Set A = 0 so we can start building the answer in A

 LDX #8                 \ Set up a counter in X to count the 8 bits in P

 LSR P                  \ Set P = P >> 1
                        \ and C flag = bit 0 of P

                        \ We are now going to work our way through the bits of
                        \ P, and do a shift-add for any bits that are set,
                        \ keeping the running total in A. We just did the first
                        \ shift right, so we now need to do the first add and
                        \ loop through the other bits in P

.MUL6

 BCC P%+4               \ If C (i.e. the next bit from P) is set, do the
 ADC T                  \ addition for this bit of P:
                        \
                        \   A = A + T + C
                        \     = A + X - 1 + 1
                        \     = A + X

 ROR A                  \ Shift A right to catch the next digit of our result,
                        \ which the next ROR sticks into the left end of P while
                        \ also extracting the next bit of P

 ROR P                  \ Add the overspill from shifting A to the right onto
                        \ the start of P, and shift P right to fetch the next
                        \ bit for the calculation into the C flag

 DEX                    \ Decrement the loop counter

 BNE MUL6               \ Loop back for the next bit until P has been rotated
                        \ all the way

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MU6
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Set P(1 0) = (A A)
\
\ ------------------------------------------------------------------------------
\
\ In practice this is only called via a BEQ following an AND instruction, in
\ which case A = 0, so this routine effectively does this:
\
\   P(1 0) = 0
\
\ ******************************************************************************

.MU6

 STA P+1                \ Set P(1 0) = (A A)
 STA P

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: FMLTU
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate A = A * Q / 256
\
\ ------------------------------------------------------------------------------
\
\ Do the following multiplication of two unsigned 8-bit numbers, returning only
\ the high byte of the result:
\
\   (A ?) = A * Q
\
\ or, to put it another way:
\
\   A = A * Q / 256
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   C flag              The C flag is set
\
\ ******************************************************************************

.FMLTU

 EOR #%11111111         \ Flip the bits in A, set the C flag and rotate right,
 SEC                    \ so the C flag now contains bit 0 of A inverted, and P
 ROR A                  \ contains A inverted and shifted right by one, with bit
 STA P                  \ 7 set to a 1. We can now use P as our source of bits
                        \ to shift right, just as in MU11, just with the logic
                        \ reversed

 LDA #0                 \ Set A = 0 so we can start building the answer in A

.MUL3

 BCS MU7                \ If C (i.e. the next bit from P) is set, do not do the
                        \ addition for this bit of P, and instead skip to MU7
                        \ to just do the shifts

 ADC Q                  \ Do the addition for this bit of P:
                        \
                        \   A = A + Q + C
                        \     = A + Q

 ROR A                  \ Shift A right to catch the next digit of our result.
                        \ If we were interested in the low byte of the result we
                        \ would want to save the bit that falls off the end, but
                        \ we aren't, so we can ignore it

 LSR P                  \ Shift P right to fetch the next bit for the
                        \ calculation into the C flag

 BNE MUL3               \ Loop back to MUL3 if P still contains some set bits
                        \ (so we loop through the bits of P until we get to the
                        \ 1 we inserted before the loop, and then we stop)

                        \ If we get here then the C flag is set as we just
                        \ rotated a 1 out of the right end of P

 RTS                    \ Return from the subroutine

.MU7

 LSR A                  \ Shift A right to catch the next digit of our result,
                        \ pushing a 0 into bit 7 as we aren't adding anything
                        \ here (we can't use a ROR here as the C flag is set, so
                        \ a ROR would push a 1 into bit 7)

 LSR P                  \ Fetch the next bit from P into the C flag

 BNE MUL3               \ Loop back to MUL3 if P still contains some set bits
                        \ (so we loop through the bits of P until we get to the
                        \ 1 we inserted before the loop, and then we stop)

                        \ If we get here then the C flag is set as we just
                        \ rotated a 1 out of the right end of P

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MLTU2
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A P+1 P) = (A ~P) * Q
\  Deep dive: Shift-and-add multiplication
\
\ ------------------------------------------------------------------------------
\
\ Do the following multiplication of an unsigned 16-bit number and an unsigned
\ 8-bit number:
\
\   (A P+1 P) = (A ~P) * Q
\
\ where ~P means P EOR %11111111 (i.e. P with all its bits flipped). In other
\ words, if you wanted to calculate &1234 * &56, you would:
\
\   * Set A to &12
\   * Set P to &34 EOR %11111111 = &CB
\   * Set Q to &56
\
\ before calling MLTU2.
\
\ This routine is like a mash-up of MU11 and FMLTU. It uses part of FMLTU's
\ inverted argument trick to work out whether or not to do an addition, and like
\ MU11 it sets up a counter in X to extract bits from (P+1 P). But this time we
\ extract 16 bits from (P+1 P), so the result is a 24-bit number. The core of
\ the algorithm is still the shift-and-add approach explained in MULT1, just
\ with more bits.
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   Q                   Q is preserved
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   MLTU2-2             Set Q to X, so this calculates (A P+1 P) = (A ~P) * X
\
\ ******************************************************************************

 STX Q                  \ Store X in Q

.MLTU2

 EOR #%11111111         \ Flip the bits in A and rotate right, storing the
 LSR A                  \ result in P+1, so we now calculate (P+1 P) * Q
 STA P+1

 LDA #0                 \ Set A = 0 so we can start building the answer in A

 LDX #16                \ Set up a counter in X to count the 16 bits in (P+1 P)

 ROR P                  \ Set P = P >> 1 with bit 7 = bit 0 of A
                        \ and C flag = bit 0 of P

.MUL7

 BCS MU21               \ If C (i.e. the next bit from P) is set, do not do the
                        \ addition for this bit of P, and instead skip to MU21
                        \ to just do the shifts

 ADC Q                  \ Do the addition for this bit of P:
                        \
                        \   A = A + Q + C
                        \     = A + Q

 ROR A                  \ Rotate (A P+1 P) to the right, so we capture the next
 ROR P+1                \ digit of the result in P+1, and extract the next digit
 ROR P                  \ of (P+1 P) in the C flag

 DEX                    \ Decrement the loop counter

 BNE MUL7               \ Loop back for the next bit until P has been rotated
                        \ all the way

 RTS                    \ Return from the subroutine

.MU21

 LSR A                  \ Shift (A P+1 P) to the right, so we capture the next
 ROR P+1                \ digit of the result in P+1, and extract the next digit
 ROR P                  \ of (P+1 P) in the C flag

 DEX                    \ Decrement the loop counter

 BNE MUL7               \ Loop back for the next bit until P has been rotated
                        \ all the way

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MUT2
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (S R) = XX(1 0) and (A P) = Q * A
\
\ ------------------------------------------------------------------------------
\
\ Do the following assignment, and multiplication of two signed 8-bit numbers:
\
\   (S R) = XX(1 0)
\   (A P) = Q * A
\
\ ******************************************************************************

.MUT2

 LDX XX+1               \ Set S = XX+1
 STX S

                        \ Fall through into MUT1 to do the following:
                        \
                        \   R = XX
                        \   (A P) = Q * A

\ ******************************************************************************
\
\       Name: MUT1
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate R = XX and (A P) = Q * A
\
\ ------------------------------------------------------------------------------
\
\ Do the following assignment, and multiplication of two signed 8-bit numbers:
\
\   R = XX
\   (A P) = Q * A
\
\ ******************************************************************************

.MUT1

 LDX XX                 \ Set R = XX
 STX R

                        \ Fall through into MULT1 to do the following:
                        \
                        \   (A P) = Q * A

\ ******************************************************************************
\
\       Name: MULT1
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A P) = Q * A
\  Deep dive: Shift-and-add multiplication
\
\ ------------------------------------------------------------------------------
\
\ Do the following multiplication of two 8-bit sign-magnitude numbers:
\
\   (A P) = Q * A
\
\ ******************************************************************************

.MULT1

 TAX                    \ Store A in X

 AND #%01111111         \ Set P = |A| >> 1
 LSR A                  \ and C flag = bit 0 of A
 STA P

 TXA                    \ Restore argument A

 EOR Q                  \ Set bit 7 of A and T if Q and A have different signs,
 AND #%10000000         \ clear bit 7 if they have the same signs, 0 all other
 STA T                  \ bits, i.e. T contains the sign bit of Q * A

 LDA Q                  \ Set A = |Q|
 AND #%01111111

 BEQ mu10               \ If |Q| = 0 jump to mu10 (with A set to 0)

 TAX                    \ Set T1 = |Q| - 1
 DEX                    \
 STX T1                 \ We subtract 1 as the C flag will be set when we want
                        \ to do an addition in the loop below

                        \ We are now going to work our way through the bits of
                        \ P, and do a shift-add for any bits that are set,
                        \ keeping the running total in A. We already set up
                        \ the first shift at the start of this routine, as
                        \ P = |A| >> 1 and C = bit 0 of A, so we now need to set
                        \ up a loop to sift through the other 7 bits in P

 LDA #0                 \ Set A = 0 so we can start building the answer in A

 LDX #7                 \ Set up a counter in X to count the 7 bits remaining
                        \ in P

.MUL4

 BCC P%+5               \ If C (i.e. the next bit from P) is set, do the
 ADC T1                 \ addition for this bit of P:
                        \
                        \   A = A + T1 + C
                        \     = A + |Q| - 1 + 1
                        \     = A + |Q|

 ROR A                  \ As mentioned above, this ROR shifts A right and
                        \ catches bit 0 in C - giving another digit for our
                        \ result - and the next ROR sticks that bit into the
                        \ left end of P while also extracting the next bit of P
                        \ for the next addition

 ROR P                  \ Add the overspill from shifting A to the right onto
                        \ the start of P, and shift P right to fetch the next
                        \ bit for the calculation

 DEX                    \ Decrement the loop counter

 BNE MUL4               \ Loop back for the next bit until P has been rotated
                        \ all the way

 LSR A                  \ Rotate (A P) once more to get the final result, as
 ROR P                  \ we only pushed 7 bits through the above process

 ORA T                  \ Set the sign bit of the result that we stored in T

 RTS                    \ Return from the subroutine

.mu10

 STA P                  \ If we get here, the result is 0 and A = 0, so set
                        \ P = 0 so (A P) = 0

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: MULT12
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (S R) = Q * A
\
\ ------------------------------------------------------------------------------
\
\ Calculate:
\
\   (S R) = Q * A
\
\ ******************************************************************************

.MULT12

 JSR MULT1              \ Set (A P) = Q * A

 STA S                  \ Set (S R) = (A P)
 LDA P                  \           = Q * A
 STA R

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: TAS3
\       Type: Subroutine
\   Category: Maths (Geometry)
\    Summary: Calculate the dot product of XX15 and an orientation vector
\
\ ------------------------------------------------------------------------------
\
\ Calculate the dot product of the vector in XX15 and one of the orientation
\ vectors, as determined by the value of Y. If vect is the orientation vector,
\ we calculate this:
\
\   (A X) = vect . XX15
\         = vect_x * XX15 + vect_y * XX15+1 + vect_z * XX15+2
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   Y                   The orientation vector:
\
\                         * If Y = 10, calculate nosev . XX15
\
\                         * If Y = 16, calculate roofv . XX15
\
\                         * If Y = 22, calculate sidev . XX15
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   (A X)               The result of the dot product
\
\ ******************************************************************************

.TAS3

 LDX INWK,Y             \ Set Q = the Y-th byte of INWK, i.e. vect_x
 STX Q

 LDA XX15               \ Set A = XX15

 JSR MULT12             \ Set (S R) = Q * A
                        \           = vect_x * XX15

 LDX INWK+2,Y           \ Set Q = the Y+2-th byte of INWK, i.e. vect_y
 STX Q

 LDA XX15+1             \ Set A = XX15+1

 JSR MAD                \ Set (A X) = Q * A + (S R)
                        \           = vect_y * XX15+1 + vect_x * XX15

 STA S                  \ Set (S R) = (A X)
 STX R

 LDX INWK+4,Y           \ Set Q = the Y+2-th byte of INWK, i.e. vect_z
 STX Q

 LDA XX15+2             \ Set A = XX15+2

                        \ Fall through into MAD to set:
                        \
                        \   (A X) = Q * A + (S R)
                        \           = vect_z * XX15+2 + vect_y * XX15+1 +
                        \             vect_x * XX15

\ ******************************************************************************
\
\       Name: MAD
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A X) = Q * A + (S R)
\
\ ------------------------------------------------------------------------------
\
\ Calculate
\
\   (A X) = Q * A + (S R)
\
\ ******************************************************************************

.MAD

 JSR MULT1              \ Call MULT1 to set (A P) = Q * A

                        \ Fall through into ADD to do:
                        \
                        \   (A X) = (A P) + (S R)
                        \         = Q * A + (S R)

\ ******************************************************************************
\
\       Name: ADD
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A X) = (A P) + (S R)
\  Deep dive: Adding sign-magnitude numbers
\
\ ------------------------------------------------------------------------------
\
\ Add two 16-bit sign-magnitude numbers together, calculating:
\
\   (A X) = (A P) + (S R)
\
\ ******************************************************************************

.ADD

 STA T1                 \ Store argument A in T1

 AND #%10000000         \ Extract the sign (bit 7) of A and store it in T
 STA T

 EOR S                  \ EOR bit 7 of A with S. If they have different bit 7s
 BMI MU8                \ (i.e. they have different signs) then bit 7 in the
                        \ EOR result will be 1, which means the EOR result is
                        \ negative. So the AND, EOR and BMI together mean "jump
                        \ to MU8 if A and S have different signs"

                        \ If we reach here, then A and S have the same sign, so
                        \ we can add them and set the sign to get the result

 LDA R                  \ Add the least significant bytes together into X:
 CLC                    \
 ADC P                  \   X = P + R
 TAX

 LDA S                  \ Add the most significant bytes together into A. We
 ADC T1                 \ stored the original argument A in T1 earlier, so we
                        \ can do this with:
                        \
                        \   A = A  + S + C
                        \     = T1 + S + C

 ORA T                  \ If argument A was negative (and therefore S was also
                        \ negative) then make sure result A is negative by
                        \ OR'ing the result with the sign bit from argument A
                        \ (which we stored in T)

 RTS                    \ Return from the subroutine

.MU8

                        \ If we reach here, then A and S have different signs,
                        \ so we can subtract their absolute values and set the
                        \ sign to get the result

 LDA S                  \ Clear the sign (bit 7) in S and store the result in
 AND #%01111111         \ U, so U now contains |S|
 STA U

 LDA P                  \ Subtract the least significant bytes into X:
 SEC                    \
 SBC R                  \   X = P - R
 TAX

 LDA T1                 \ Restore the A of the argument (A P) from T1 and
 AND #%01111111         \ clear the sign (bit 7), so A now contains |A|

 SBC U                  \ Set A = |A| - |S|

                        \ At this point we have |A P| - |S R| in (A X), so we
                        \ need to check whether the subtraction above was the
                        \ right way round (i.e. that we subtracted the smaller
                        \ absolute value from the larger absolute value)

 BCS MU9                \ If |A| >= |S|, our subtraction was the right way
                        \ round, so jump to MU9 to set the sign

                        \ If we get here, then |A| < |S|, so our subtraction
                        \ above was the wrong way round (we actually subtracted
                        \ the larger absolute value from the smaller absolute
                        \ value). So let's subtract the result we have in (A X)
                        \ from zero, so that the subtraction is the right way
                        \ round

 STA U                  \ Store A in U

 TXA                    \ Set X = 0 - X using two's complement (to negate a
 EOR #&FF               \ number in two's complement, you can invert the bits
 ADC #1                 \ and add one - and we know the C flag is clear as we
 TAX                    \ didn't take the BCS branch above, so the ADC will do
                        \ the correct addition)

 LDA #0                 \ Set A = 0 - A, which we can do this time using a
 SBC U                  \ subtraction with the C flag clear

 ORA #%10000000         \ We now set the sign bit of A, so that the EOR on the
                        \ next line will give the result the opposite sign to
                        \ argument A (as T contains the sign bit of argument
                        \ A). This is the same as giving the result the same
                        \ sign as argument S (as A and S have different signs),
                        \ which is what we want, as S has the larger absolute
                        \ value

.MU9

 EOR T                  \ If we get here from the BCS above, then |A| >= |S|,
                        \ so we want to give the result the same sign as
                        \ argument A, so if argument A was negative, we flip
                        \ the sign of the result with an EOR (to make it
                        \ negative)

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: TIS1
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (A ?) = (-X * A + (S R)) / 96
\  Deep dive: Shift-and-subtract division
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following expression between sign-magnitude numbers, ignoring
\ the low byte of the result:
\
\   (A ?) = (-X * A + (S R)) / 96
\
\ This uses the same shift-and-subtract algorithm as TIS2, just with the
\ quotient A hard-coded to 96.
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   Q                   Gets set to the value of argument X
\
\ ******************************************************************************

.TIS1

 STX Q                  \ Set Q = X

 EOR #%10000000         \ Flip the sign bit in A

 JSR MAD                \ Set (A X) = Q * A + (S R)
                        \           = X * -A + (S R)

.DVID96

 TAX                    \ Set T to the sign bit of the result
 AND #%10000000
 STA T

 TXA                    \ Set A to the high byte of the result with the sign bit
 AND #%01111111         \ cleared, so (A ?) = |X * A + (S R)|

                        \ The following is identical to TIS2, except Q is
                        \ hard-coded to 96, so this does A = A / 96

 LDX #254               \ Set T1 to have bits 1-7 set, so we can rotate through
 STX T1                 \ 7 loop iterations, getting a 1 each time, and then
                        \ getting a 0 on the 8th iteration... and we can also
                        \ use T1 to catch our result bits into bit 0 each time

.DVL3

 ASL A                  \ Shift A to the left

 CMP #96                \ If A < 96 skip the following subtraction
 BCC DV4

 SBC #96                \ Set A = A - 96
                        \
                        \ Going into this subtraction we know the C flag is
                        \ set as we passed through the BCC above, and we also
                        \ know that A >= 96, so the C flag will still be set
                        \ once we are done

.DV4

 ROL T1                 \ Rotate the counter in T1 to the left, and catch the
                        \ result bit into bit 0 (which will be a 0 if we didn't
                        \ do the subtraction, or 1 if we did)

 BCS DVL3               \ If we still have set bits in T1, loop back to DVL3 to
                        \ do the next iteration of 7

 LDA T1                 \ Fetch the result from T1 into A

 ORA T                  \ Give A the sign of the result that we stored above

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: GINF
\       Type: Subroutine
\   Category: Universe
\    Summary: Fetch the address of a ship's data block into INF
\
\ ------------------------------------------------------------------------------
\
\ Get the address of the data block for ship slot X and store it in INF. This
\ address is fetched from the UNIV table, which stores the addresses of the 13
\ ship data blocks in workspace K%.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   The ship slot number for which we want the data block
\                       address
\
\ ******************************************************************************

.GINF

 TXA                    \ Set Y = X * 2
 ASL A
 TAY

 LDA UNIV,Y             \ Get the high byte of the address of the X-th ship
 STA INF                \ from UNIV and store it in INF

 LDA UNIV+1,Y           \ Get the low byte of the address of the X-th ship
 STA INF+1              \ from UNIV and store it in INF

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: NWSHP
\       Type: Subroutine
\   Category: Universe
\    Summary: Add a new ship to our local bubble of universe
\
\ ------------------------------------------------------------------------------
\
\ This creates a new block of ship data in the K% workspace, allocates a new
\ block in the ship line heap at WP, adds the new ship's type into the first
\ empty slot in FRIN, and adds a pointer to the ship data into UNIV. If there
\ isn't enough free memory for the new ship, it isn't added.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   A                   The type of the ship to add (see variable XX21 for a
\                       list of ship types)
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   C flag              Set if the ship was successfully added, clear if it
\                       wasn't (as there wasn't enough free memory)
\
\   INF                 Points to the new ship's data block in K%
\
\ ******************************************************************************

.NWSHP

 STA T                  \ Store the ship type in location T

 LDX #0                 \ Before we can add a new ship, we need to check
                        \ whether we have an empty slot we can put it in. To do
                        \ this, we need to loop through all the slots to look
                        \ for an empty one, so set a counter in X that starts
                        \ from the first slot at 0. When ships are killed, then
                        \ the slots are shuffled down by the KILLSHP routine, so
                        \ the first empty slot will always come after the last
                        \ filled slot. This allows us to tack the new ship's
                        \ data block and ship line heap onto the end of the
                        \ existing ship data and heap, as shown in the memory
                        \ map below

.NWL1

 LDA FRIN,X             \ Load the ship type for the X-th slot

 BEQ NW1                \ If it is zero, then this slot is empty and we can use
                        \ it for our new ship, so jump down to NW1

 INX                    \ Otherwise increment X to point to the next slot

 CPX #NOSH              \ If we haven't reached the last slot yet, loop back up
 BCC NWL1               \ to NWL1 to check the next slot (note that this means
                        \ only slots from 0 to #NOSH - 1 are populated by this
                        \ routine, but there is one more slot reserved in FRIN,
                        \ which is used to identify the end of the slot list
                        \ when shuffling the slots down in the KILLSHP routine)

.NW3

 CLC                    \ Otherwise we don't have an empty slot, so we can't
 RTS                    \ add a new ship, so clear the C flag to indicate that
                        \ we have not managed to create the new ship, and return
                        \ from the subroutine

.NW1

                        \ If we get here, then we have found an empty slot at
                        \ index X, so we can go ahead and create our new ship.
                        \ We do that by creating a ship data block at INWK and,
                        \ when we are done, copying the block from INWK into
                        \ the K% workspace (specifically, to INF)

 JSR GINF               \ Get the address of the data block for ship slot X
                        \ (which is in workspace K%) and store it in INF

 LDA T                  \ If the type of ship that we want to create is
 BMI NW2                \ negative, then this indicates a planet or sun, so
                        \ jump down to NW2, as the next section sets up a ship
                        \ data block, which doesn't apply to planets and suns,
                        \ as they don't have things like shields, missiles,
                        \ vertices and edges

                        \ This is a ship, so first we need to set up various
                        \ pointers to the ship blueprint we will need. The
                        \ blueprints for each ship type in Elite are stored
                        \ in a table at location XX21, so refer to the comments
                        \ on that variable for more details on the data we're
                        \ about to access

 ASL A                  \ Set Y = ship type * 2
 TAY

 LDA XX21-2,Y           \ The ship blueprints at XX21 start with a lookup
 STA XX0                \ table that points to the individual ship blueprints,
                        \ so this fetches the low byte of this particular ship
                        \ type's blueprint and stores it in XX0

 LDA XX21-1,Y           \ Fetch the high byte of this particular ship type's
 STA XX0+1              \ blueprint and store it in XX0+1, so XX0(1 0) now
                        \ contains the address of this ship's blueprint

 CPY #2*SST             \ If the ship type is a space station (SST), then jump
 BEQ NW6                \ to NW6, skipping the heap space steps below, as the
                        \ space station has its own line heap at LSO (which it
                        \ shares with the sun)

                        \ We now want to allocate space for a heap that we can
                        \ use to store the lines we draw for our new ship (so it
                        \ can easily be erased from the screen again). SLSP
                        \ points to the start of the current heap space, and we
                        \ can extend it downwards with the heap for our new ship
                        \ (as the heap space always ends just before the WP
                        \ workspace)

 LDY #5                 \ Fetch ship blueprint byte #5, which contains the
 LDA (XX0),Y            \ maximum heap size required for plotting the new ship,
 STA T1                 \ and store it in T1

 LDA SLSP               \ Take the 16-bit address in SLSP and subtract T1,
 SEC                    \ storing the 16-bit result in INWK(34 33), so this now
 SBC T1                 \ points to the start of the line heap for our new ship
 STA INWK+33
 LDA SLSP+1
 SBC #0
 STA INWK+34

                        \ We now need to check that there is enough free space
                        \ for both this new line heap and the new data block
                        \ for our ship. In memory, this is the layout of the
                        \ ship data blocks and ship line heaps:
                        \
                        \   +-----------------------------------+   &0F34
                        \   |                                   |
                        \   | WP workspace                      |
                        \   |                                   |
                        \   +-----------------------------------+   &0D40 = WP
                        \   |                                   |
                        \   | Current ship line heap            |
                        \   |                                   |
                        \   +-----------------------------------+   SLSP
                        \   |                                   |
                        \   | Proposed heap for new ship        |
                        \   |                                   |
                        \   +-----------------------------------+   INWK(34 33)
                        \   |                                   |
                        \   .                                   .
                        \   .                                   .
                        \   .                                   .
                        \   .                                   .
                        \   .                                   .
                        \   |                                   |
                        \   +-----------------------------------+   INF + NI%
                        \   |                                   |
                        \   | Proposed data block for new ship  |
                        \   |                                   |
                        \   +-----------------------------------+   INF
                        \   |                                   |
                        \   | Existing ship data blocks         |
                        \   |                                   |
                        \   +-----------------------------------+   &0900 = K%
                        \
                        \ So, to work out if we have enough space, we have to
                        \ make sure there is room between the end of our new
                        \ ship data block at INF + NI%, and the start of the
                        \ proposed heap for our new ship at the address we
                        \ stored in INWK(34 33). Or, to put it another way, we
                        \ and to make sure that:
                        \
                        \   INWK(34 33) > INF + NI%
                        \
                        \ which is the same as saying:
                        \
                        \   INWK+33 - INF > NI%
                        \
                        \ because INWK is in zero page, so INWK+34 = 0

 LDA INWK+33            \ Calculate INWK+33 - INF, again using 16-bit
 SBC INF                \ arithmetic, and put the result in (A Y), so the high
 TAY                    \ byte is in A and the low byte in Y. The subtraction
 LDA INWK+34            \ works because the previous subtraction will never
 SBC INF+1              \ underflow, so we know the C flag is set

 BCC NW3+1              \ If we have an underflow from the subtraction, then
                        \ INF > INWK+33 and we definitely don't have enough
                        \ room for this ship, so jump to NW3+1, which returns
                        \ from the subroutine (with the C flag already cleared)

 BNE NW4                \ If the subtraction of the high bytes in A is not
                        \ zero, and we don't have underflow, then we definitely
                        \ have enough space, so jump to NW4 to continue setting
                        \ up the new ship

 CPY #NI%               \ Otherwise the high bytes are the same in our
 BCC NW3+1              \ subtraction, so now we compare the low byte of the
                        \ result (which is in Y) with NI%. This is the same as
                        \ doing INWK+33 - INF > NI% (see above). If this isn't
                        \ true, the C flag will be clear and we don't have
                        \ enough space, so we jump to NW3+1, which returns
                        \ from the subroutine (with the C flag already cleared)

.NW4

 LDA INWK+33            \ If we get here then we do have enough space for our
 STA SLSP               \ new ship, so store the new bottom of the ship line
 LDA INWK+34            \ heap (i.e. INWK+33) in SLSP, doing both the high and
 STA SLSP+1             \ low bytes

.NW6

 LDY #14                \ Fetch ship blueprint byte #14, which contains the
 LDA (XX0),Y            \ ship's energy, and store it in byte #35
 STA INWK+35

 LDY #19                \ Fetch ship blueprint byte #19, which contains the
 LDA (XX0),Y            \ number of missiles and laser power, and AND with %111
 AND #%00000111         \ to extract the number of missiles before storing in
 STA INWK+31            \ byte #31

 LDA T                  \ Restore the ship type we stored above

.NW2

 STA FRIN,X             \ Store the ship type in the X-th byte of FRIN, so the
                        \ this slot is now shown as occupied in the index table

 TAX                    \ Copy the ship type into X

 BMI P%+5               \ If the ship type is negative (planet or sun), then
                        \ skip the following instruction

\INC MANY,X             \ Increment the total number of ships of type X

 LDY #NI%-1             \ The final step is to copy the new ship's data block
                        \ from INWK to INF, so set up a counter for NI% bytes
                        \ in Y

.NWL3

 LDA INWK,Y             \ Load the Y-th byte of INWK and store in the Y-th byte
 STA (INF),Y            \ of the workspace pointed to by INF

 DEY                    \ Decrement the loop counter

 BPL NWL3               \ Loop back for the next byte until we have copied them
                        \ all over

 SEC                    \ We have successfully created our new ship, so set the
                        \ C flag to indicate success

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: RESET
\       Type: Subroutine
\   Category: Start and end
\    Summary: Reset most variables
\
\ ------------------------------------------------------------------------------
\
\ Reset our ship and various controls, recharge shields and energy, and then
\ fall through into RES2 to reset the stardust and the ship workspace at INWK.
\
\ ******************************************************************************

.RESET

 JSR ZERO               \ Reset the ship slots

 LDX #0                 \ Zero the rotation angles
 STX BETA

                        \ Fall through into RES2 to reset the stardust and ship
                        \ workspace at INWK

\ ******************************************************************************
\
\       Name: RES2
\       Type: Subroutine
\   Category: Start and end
\    Summary: Reset a number of flight variables and workspaces
\
\ ------------------------------------------------------------------------------
\
\ This is called after we launch from a space station, arrive in a new system
\ after hyperspace, launch an escape pod, or die a cold, lonely death in the
\ depths of space.
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   Y                   Y is set to &FF
\
\ ******************************************************************************

.RES2

 LDA #0
 STA MCNT               \ Reset MCNT (the main loop counter) to 0

.modify

 STA ALPHA              \ Reset ALPHA (roll angle alpha) to 3

 JSR ZERO               \ Reset the ship slots

 LDA #LO(LS%)           \ We have reset the ship line heap, so we now point
 STA SLSP               \ SLSP to LS% (the byte below the ship blueprints at D%)
 LDA #HI(LS%)           \ to indicate that the heap is empty
 STA SLSP+1

                        \ Finally, fall through into ZINF to reset the INWK
                        \ ship workspace

\ ******************************************************************************
\
\       Name: ZINF
\       Type: Subroutine
\   Category: Universe
\    Summary: Reset the INWK workspace and orientation vectors
\  Deep dive: Orientation vectors
\
\ ------------------------------------------------------------------------------
\
\ Zero-fill the INWK ship workspace and reset the orientation vectors, with
\ nosev pointing out of the screen, towards us.
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   Y                   Y is set to &FF
\
\ ******************************************************************************

.ZINF

 LDY #NI%-1             \ There are NI% bytes in the INWK workspace, so set a
                        \ counter in Y so we can loop through them

 LDA #0                 \ Set A to 0 so we can zero-fill the workspace

.ZI1

 STA INWK,Y             \ Zero the Y-th byte of the INWK workspace

 DEY                    \ Decrement the loop counter

 BPL ZI1                \ Loop back for the next byte, ending when we have
                        \ zero-filled the last byte at INWK, which leaves Y
                        \ with a value of &FF

                        \ Finally, we reset the orientation vectors as follows:
                        \
                        \   sidev = (1,  0,  0)
                        \   roofv = (0,  1,  0)
                        \   nosev = (0,  0, -1)
                        \
                        \ 96 * 256 (&6000) represents 1 in the orientation
                        \ vectors, while -96 * 256 (&E000) represents -1. We
                        \ already set the vectors to zero above, so we just
                        \ need to set up the high bytes of the diagonal values
                        \ and we're done. The negative nosev makes the ship
                        \ point towards us, as the z-axis points into the screen

 LDA #96                \ Set A to represent a 1 (in vector terms)

 STA INWK+18            \ Set byte #18 = roofv_y_hi = 96 = 1

 STA INWK+22            \ Set byte #22 = sidev_x_hi = 96 = 1

 ORA #%10000000         \ Flip the sign of A to represent a -1

 STA INWK+14            \ Set byte #14 = nosev_z_hi = -96 = -1

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: BEGIN
\       Type: Subroutine
\   Category: Server menu
\    Summary: Show the menu
\
\ ******************************************************************************

.BEGIN

\LDX #&FF               \ Set the stack pointer to &01FF, which is the standard
\TXS                    \ location for the 6502 stack, so this instruction
                        \ effectively resets the stack

 JSR FX200              \ Disable the ESCAPE key and clear memory if the BREAK
                        \ key is pressed (*FX 200,3)

 LDA #4                 \ Call OSBYTE with A = 4, X = 1 and Y = 0 to disable
 LDX #1                 \ cursor editing, so the cursor keys return ASCII values
 LDY #0                 \ and can therefore be used in-game
 JSR OSBYTE

 JSR ClearMode7Screen   \ Clear the screen

 JSR SetMode7Graphics   \ Set all screen rows to white graphics

 TEXT_AT 12, 0, titleText   \ Display the title

.begn1

 JSR PrintMainMenu      \ Print the main menu

 LDY selection          \ Highlight the currently selected menu item
 JSR AddHighlight

 LDA #0                 \ Set the menu type to 0, for the main menu
 STA menuType

 LDX #CYL               \ Set TYPE to show a rotating Cobra Mk III (#CYL) in the
 STX TYPE               \ call to TITLE

 LDX #96                \ Set shipDistance = 96 for the Cobra
 STX shipDistance

 JSR TITLE              \ Call TITLE to show a rotating Cobra Mk III with the
                        \ main menu

 JSR PrintInfo          \ Print the information for the selection

 LDY selection          \ Set TYPE to the correct ship for this menu
 LDA menuShip,Y
 STA TYPE

 LDA shipDistance,Y     \ Set shipDistance to the correct distance for this menu
 STA shipDistance

 LDA #1                 \ Set the menu type to 1, for the info menu
 STA menuType

 JSR TITLE              \ Call TITLE to show a rotating ship with the info for
                        \ the selected item from the main menu

 LDA menuType           \ If menuType has been changed back to zero, then we
 BNE begn2              \ need to show the main menu, so jump to begn1 to do so
 JMP begn1

.begn2

 LDA selection          \ If menu item 1 was not chosen, jump to begn3 to check
 CMP #1                 \ the next item
 BNE begn3

                        \ If we get here then menu item 1 was chosen

 LDX #LO(command1)      \ Set (Y X) to point to command1
 LDY #HI(command1)

 JSR OSCLI              \ Call OSCLI to run the OS command in command1, which
                        \ sets the f0 key to run flicker-free Elite

 LDX #LO(command1a)      \ Set (Y X) to point to command1
 LDY #HI(command1a)

 LDA #138
 LDX #0
 LDY #128
 JSR OSBYTE

 RTS

.begn3
                        \ Otherwise we have chosen to run the selected menu
                        \ item, so that's what we do now

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: AddHighlight
\       Type: Subroutine
\   Category: Server menu
\    Summary: Add a highlight to a menu item
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   Y                   The number of the menu item
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   A                   A is preserved
\
\   Y                   Y is preserved
\
\ ******************************************************************************

.AddHighlight

 PHA                    \ Store A on the stack

 TYA                    \ Store the number of the menu item on the stack
 PHA

 ASL A                  \ Set A = Y * 2 so we can use it as an index into the
 TAY                    \ address table at highlightAddr

 LDA highlightAddr,Y    \ Set V(1 0) to the highlightAddr entry for the menu
 STA V                  \ item, which contains the screen address of the item's
 LDA highlightAddr+1,Y  \ highlight characters
 STA V+1

 LDY #0                 \ Set the characters to 131, "]" to highlight the item
 LDA #131
 STA (V),Y
 LDA #']'
 INY
 STA (V),Y

 PLA                    \ Restore the number of the menu item in Y
 TAY

 PLA                    \ Restore A from the stack

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: RemoveHighlight
\       Type: Subroutine
\   Category: Server menu
\    Summary: Remove the highlight from a menu item
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   Y                   The number of the menu item
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   A                   A is preserved
\
\   Y                   Y is preserved
\
\ ******************************************************************************

.RemoveHighlight

 PHA                    \ Store A on the stack

 TYA                    \ Store the number of the menu item on the stack
 PHA

 ASL A                  \ Set A = Y * 2 so we can use it as an index into the
 TAY                    \ address table at highlightAddr

 LDA highlightAddr,Y    \ Set V(1 0) to the highlightAddr entry for the menu
 STA V                  \ item, which contains the screen address of the item's
 LDA highlightAddr+1,Y  \ highlight characters
 STA V+1

 LDY #0                 \ Set the characters to 134, " " to clear the item
 LDA #134
 STA (V),Y
 LDA #' '
 INY
 STA (V),Y

 PLA                    \ Restore the number of the menu item in Y
 TAY

 PLA                    \ Restore A from the stack

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: PrintMainMenu
\       Type: Subroutine
\   Category: Server menu
\    Summary: Print the main menu
\
\ ******************************************************************************

.PrintMainMenu

 TEXT_AT 0, 16, menuKey \ Display the menu key

 TEXT_AT 0, 18, menu1   \ Display the menu
 TEXT_AT 0, 20, menu2
 TEXT_AT 0, 22, menu3
 TEXT_AT 0, 24, menu4

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: PrintInfo
\       Type: Subroutine
\   Category: Server menu
\    Summary: Print the info text for the selected menu
\
\ ******************************************************************************

.PrintInfo

 TEXT_AT 0, 24, infoKey \ Display the info key

 LDA selection          \ Set Y = selection * 2 so we can use it as an index
 ASL A                  \ into the address table at infoAddr
 TAY

 LDA #LO(MODE7_VRAM+(16*&28))   \ Print the info text in the menu area
 STA V
 LDA #HI(MODE7_VRAM+(16*&28))
 STA V+1

 LDA infoAddr,Y         \ Set P(1 0) to the infoAddr entry for the menu
 STA P                  \ item, which contains the address of the item's info
 LDA infoAddr+1,Y       \ text
 STA P+1

 LDY #0                 \ Sey Y = 0 to act as an index into the string

.pinf1

 LDA (P),Y              \ Copy the Y-th byte of the message from P(1 0)

 BEQ pinf2              \ If it is zero, jump to pinf2 to return from the
                        \ subroutine

 STA (V),Y              \ Poke the byte into screen memory in V(1 0)

 INY                    \ Increment the index

 BNE pinf1              \ Loop back to print the next character    

 INC V+1                \ Move on to the next page
 INC P+1

 BNE pinf1              \ Loop back to print the next character

.pinf2

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: highlightAddr
\       Type: Variable
\   Category: Server menu
\    Summary: The address of the highlight characters for each of the eight menu
\             items in the main menu
\
\ ******************************************************************************

.highlightAddr

 EQUW MODE7_VRAM+(18*&28)+0
 EQUW MODE7_VRAM+(20*&28)+0
 EQUW MODE7_VRAM+(22*&28)+0
 EQUW MODE7_VRAM+(24*&28)+0
 EQUW MODE7_VRAM+(18*&28)+20
 EQUW MODE7_VRAM+(20*&28)+20
 EQUW MODE7_VRAM+(22*&28)+20
 EQUW MODE7_VRAM+(24*&28)+20

\ ******************************************************************************
\
\       Name: selection
\       Type: Variable
\   Category: Server menu
\    Summary: The number of the currently selected menu item (0 to 7, counting
\             from top-left and down, then top-right and down)
\
\ ******************************************************************************

.selection

 EQUB 0                 \ The currently selected menu item

\ ******************************************************************************
\
\       Name: debounce
\       Type: Variable
\   Category: Server menu
\    Summary: A variable to track whether a key press has been released
\
\ ******************************************************************************

.debounce

 EQUB 0                 \ The currently pressed key

\ ******************************************************************************
\
\       Name: menuType
\       Type: Variable
\   Category: Server menu
\    Summary: A variable to define whether this is the main menu or an info menu
\
\ ******************************************************************************

.menuType

 EQUB 0                 \ 0 = main menu, 1 = info menu

\ ******************************************************************************
\
\       Name: menuShip
\       Type: Variable
\   Category: Server menu
\    Summary: The ship type for each menu item
\
\ ******************************************************************************

.menuShip

 EQUB CYL
 EQUB KRA
 EQUB OIL
 EQUB SHU
 EQUB 10
 EQUB 12
 EQUB COPS
 EQUB CON

\ ******************************************************************************
\
\       Name: menuDistance
\       Type: Variable
\   Category: Server menu
\    Summary: The z_lo ship distance for each menu item
\
\ ******************************************************************************

.menuDistance

 EQUB 128
 EQUB 128
 EQUB 128
 EQUB 128
 EQUB 128
 EQUB 128
 EQUB 128
 EQUB 128

\ ******************************************************************************
\
\       Name: shipDistance
\       Type: Variable
\   Category: Server menu
\    Summary: A variable to hold the nearest ship distance for the current menu
\
\ ******************************************************************************

.shipDistance

 EQUB 0

\ ******************************************************************************
\
\       Name: Menu text
\       Type: Variable
\   Category: Server menu
\    Summary: The text of the main menu
\
\ ------------------------------------------------------------------------------
\
\ 156 = black background
\ x, 157 = new background x
\ 129 = red, 131 = yellow, 132 = blue, 134 = cyan
\
\ Highlight an entry by setting the first two bytes to 131, "]"
\
\ Restore an entry by setting the first two bytes to 134, " "
\
\ ******************************************************************************

.menuKey

 EQUS 132, 157, 131, " Select with [^], RETURN for info", 0

.menu1

 EQUS 134, " ", "Flicker-free Elite"

.menu1a

 EQUS 134, " ", "Econet scoreboard", 0

.menu2

 EQUS 134, " ", "Original Elite    "

.menu2a

 EQUS 134, " ", "Econet debugger", 0

.menu3

 EQUS 134, " ", "Executive Elite   "

.menu3a

 EQUS 134, " ", "Version info", 0

.menu4

 EQUS 134, " ", "Archimedes Elite  "

.menu4a

 EQUS 134, " ", "About Econet Elite", 0

\ ******************************************************************************
\
\       Name: infoAddr
\       Type: Variable
\   Category: Server menu
\    Summary: The address of the info text for each menu item
\
\ ******************************************************************************

.infoAddr

 EQUW info1
 EQUW info2
 EQUW info3
 EQUW info4
 EQUW info5
 EQUW info6
 EQUW info7
 EQUW info8

\ ******************************************************************************
\
\       Name: Info text
\       Type: Variable
\   Category: Server menu
\    Summary: The info text for each option
\
\ ******************************************************************************

.infoKey

 EQUS 132, 157, 131, " ESCAPE to go back, RETURN to run", 0

.info1

 EQUS 132, 157, 131, "Play flicker-free Elite over Econet  "
 EQUS 134, "                                       "
 EQUS 134, "The recommended option. Play Elite over"
 EQUS 134, "Econet and join online competitions,   "
 EQUS 134, "with flicker-free ships and planets.   "
 EQUS 134, "                                       "
 EQUS 132, " For BBC Micro, BBC Master and 6502SP  "
 EQUB 0

.info2

 EQUS 132, 157, 131, "Play original Elite over Econet      "
 EQUS 134, "                                       "
 EQUS 134, "If you like your graphics authentically"
 EQUS 134, "flickery, and with the 6502SP version  "
 EQUS 134, "running at full speed, this is for you."
 EQUS 134, "                                       "
 EQUS 132, " For BBC Micro, BBC Master and 6502SP  "
 EQUB 0

.info3

 EQUS 132, 157, 131, "Play Executive Elite over Econet     "
 EQUS 134, "                                       "
 EQUS 134, "The flicker-free version of Executive  "
 EQUS 134, "Elite over Econet, with its fancy font "
 EQUS 134, "and the famous Pizzasoft scroll text.  "
 EQUS 134, "                                       "
 EQUS 132, " For the 6502 Second Processor only    "
 EQUB 0

.info4

 EQUS 132, 157, 131, "Play Archimedes Elite over Econet    "
 EQUS 134, "                                       "
 EQUS 134, "Many think this is the best version of "
 EQUS 134, "Elite, and now RISC OS players can join"
 EQUS 134, "online competitions alongside BBC Micro"
 EQUS 134, "commanders. Choose this option for info"
 EQUS 134, "on how to log in from an Archimedes.   "
 EQUB 0

.info5

 EQUS 132, 157, 131, "Run the Elite over Econet scoreboard "
 EQUS 134, "                                       "
 EQUS 134, "Host your very own Elite scoreboard    "
 EQUS 134, "with this option. See the website at   "
 EQUS 134, "bbcelite.com/econet for more details.  "
 EQUS 134, "                                       "
 EQUS 132, " For BBC Micro, BBC Master and 6502SP  "
 EQUB 0

.info6

 EQUS 132, 157, 131, "Run the Elite over Econet debugger   "
 EQUS 134, "                                       "
 EQUS 134, "Use the debugger to test your network  "
 EQUS 134, "for playing Elite. See the website at  "
 EQUS 134, "bbcelite.com/econet for more details.  "
 EQUS 134, "                                       "
 EQUS 132, " For BBC Micro, BBC Master and 6502SP  "
 EQUB 0

.info7

 EQUS 132, 157, 131, "Elite over Econet version details    "
 EQUS 134, "                                       "
 EQUS 134, "Choose this option to run a *ELITE V   "
 EQUS 134, "command that shows the build date for  "
 EQUS 134, "the version of Elite on this server.   "
 EQUS 134, "                                       "
 EQUS 134, "You will then return to this menu.     "
 EQUB 0

.info8

 EQUS 132, 157, 131, "More About Elite over Econet         "
 EQUS 134, "                                       "
 EQUS 134, "Find out more about the Elite over     "
 EQUS 134, "Econet project, and where to go for    "
 EQUS 134, "installation instructions, download    "
 EQUS 134, "links, technical information, playing  "
 EQUS 134, "tips and lots more.                    "
 EQUB 0

.infoArc

 EQUS 141, 130, "    Archimedes Elite over Econet       "
 EQUS 141, 130, "    Archimedes Elite over Econet       "
 EQUS "                                        "
 EQUS "To play Archimedes Elite over Econet,   "
 EQUS "do the following:                       "
 EQUS "                                        "
 EQUS " * In the RISC OS Desktop, use the      "
 EQUS "   Econet fileserver icon to log in to  "
 EQUS "   server 63.13 as user ARCELITE (there "
 EQUS "   is no password).                     "
 EQUS "                                        "
 EQUS " * Then follow the instructions in the  "
 EQUS "   !ReadMe file (double-click to load). "
 EQUS "                                        "
 EQUS "Archimedes Elite over Econet runs on    "
 EQUS "RISC OS 3 and above.                    "

.infoAbout

 EQUS 141, 130, "      About Elite over Econet          "
 EQUS 141, 130, "      About Elite over Econet          "
 EQUS "                                        "
 EQUS "In the old days, Elite didn't work over "
 EQUS "an Econet network. Those days are gone! "
 EQUS "                                        "
 EQUS "Not only does Elite now load from all   "
 EQUS "Econet fileservers, but you can join    "
 EQUS "multiplayer competitions and compete    "
 EQUS "for the highest scores against players  "
 EQUS "from all over the world.                "
 EQUS "                                        "
 EQUS "This server always hosts the very       "
 EQUS "latest version of the game, for the     "
 EQUS "BBC Micro, BBC Master, 6502 Second      "
 EQUS "Processor and Acorn Archimedes.         "
 EQUS "                                        "
 EQUS "For more information, visit the project"
 EQUS "website at", 129, "bbcelite.com/econet          "

.command1

 EQUS "BASIC", 13

.command1a

 EQUS "KEY0*I AM ELITE|M", 13

.command2

 EQUS "*I AM ELITEO", 13

.command3

 EQUS "*I AM ELITEX", 13

.command56

 EQUS "*I AM UTILS", 13

.command5

 EQUS "*ELITE S", 13

.command6

 EQUS "*ELITE D", 13

.command7

 EQUS "*ELITE V", 13

\ ******************************************************************************
\
\       Name: TITLE
\       Type: Subroutine
\   Category: Start and end
\    Summary: Display a menu screen with a rotating ship
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   The type of the ship to show (see variable XX21 for a
\                       list of ship types)
\
\ ******************************************************************************

.TITLE

 JSR RESET              \ Reset our ship so we can use it for the rotating
                        \ title ship

 LDA #96                \ Set nosev_z hi = 96 (96 is the value of unity in the
 STA INWK+14            \ rotation vector)

 STA INWK+7             \ Set z_hi, the high byte of the ship's z-coordinate,
                        \ to 96, which is the distance at which the rotating
                        \ ship starts out before coming towards us

 LDX #127               \ Set roll counter = 127, so don't dampen the roll and
 STX INWK+29            \ make the roll direction clockwise

 STX INWK+30            \ Set pitch counter = 127, so don't dampen the pitch and
                        \ set the pitch direction to dive

 LDA TYPE               \ Set up a new ship, using the ship type in TYPE
 JSR NWSHP

.TLL2

 LDA INWK+7             \ If z_hi (the ship's distance) is 1, jump to TL1 to
 CMP #1                 \ skip the following decrement
 BEQ TL1

 DEC INWK+7             \ Decrement the ship's distance, to bring the ship
                        \ a bit closer to us

.TL1

 JSR MVEIT              \ Move the ship in space according to the orientation
                        \ vectors and the new value in z_hi

 LDA shipDistance       \ Set z_lo = 128, so the closest the ship gets to us is
 STA INWK+6             \ z_hi = 1, z_lo = 128, or 256 + 128 = 384

 LDA #0                 \ Set x_lo = 0, so the ship remains in the screen centre
 STA INWK

 LDA #10                \ Set y_lo = 10, so the ship remains slightly above the
 STA INWK+3             \ screen centre

 JSR LL9                \ Call LL9 to display the ship

 DEC MCNT               \ Decrement the main loop counter

 JSR RDKEY              \ Scan the keyboard for a key press

 BNE tite1              \ If a key is being pressed, jump to tite1 to process it

 STA debounce           \ No key is being pressed and A = 0, so zero debounce so
                        \ we detect key presses once more

 BEQ TLL2               \ No key was pressed, so loop back up to move/rotate
                        \ the ship and check again for a key press

.tite1

 CMP debounce           \ If the key press is still the same key being held down
 BEQ TLL2               \ from last time, ignore it and loop back to keep moving
                        \ the ship

                        \ Otherwise this is not the same key being held down, so
                        \ process it

 LDX menuType           \ If menuType = 1 then this is an info screen, so jump
 CPX #1                 \ to tite12 to process the key press
 BNE P%+5
 JMP tite12

 CMP #&29               \ If the down arrow is not being pressed, jump to tite4
 BNE tite4              \ to move on to the next key press

                        \ If we get here then the down arrow is being pressed

 LDY selection          \ Set Y to the number of the currently selected item

 JSR RemoveHighlight    \ Remove the highlight from the currently selected item

 INY                    \ Increment Y to move down to the next menu item

 CPY #4                 \ If Y = 4 then set Y = 0 so we wrap around the left
 BNE tite2              \ column
 LDY #0

 BEQ tite3              \ Skip the following (this BNE is effectively a JMP as
                        \ Y is always zero)

.tite2

 CPY #8                 \ If Y = 8 then set Y = 4 so we wrap around the right
 BNE tite3              \ column
 LDY #4

.tite3

 STY selection          \ Update the number of the currently selected item

 JSR AddHighlight       \ Highlight the newly selected item

 JMP tite9              \ Jump down to tite9 to wait for the key press to be
                        \ lifted and keep going

.tite4

 CMP #&39               \ If the up arrow is not being pressed, jump to tite7
 BNE tite7              \ to move on to the next key press

                        \ If we get here then the up arrow is being pressed

 LDY selection          \ Set Y to the number of the currently selected item

 JSR RemoveHighlight    \ Remove the highlight from the currently selected item

 DEY                    \ Decrement Y to move up to the previous menu item

 CPY #&FF               \ If Y = -1 then set Y = 3 so we wrap around the left
 BNE tite5              \ column
 LDY #3

 BNE tite6              \ Skip the following (this BNE is effectively a JMP as
                        \ Y is never zero)

.tite5

 CPY #3                 \ If Y = 3 then set Y = 7 so we wrap around the right
 BNE tite6              \ column
 LDY #7

.tite6

 STY selection          \ Update the number of the currently selected item

 JSR AddHighlight       \ Highlight the newly selected item

 JMP tite9              \ Jump down to tite9 to wait for the key press to be
                        \ lifted and keep going

.tite7

 CMP #&19               \ If the left arrow is being pressed, jump to tite8 to
 BEQ tite8              \ process the key press

 CMP #&79               \ If the right arrow is not being pressed, jump to tite9
 BNE tite9              \ to move on to the next key press

.tite8

                        \ If we get here then either the left or right arrow is
                        \ being pressed

 LDY selection          \ Set Y to the number of the currently selected item

 JSR RemoveHighlight    \ Remove the highlight from the currently selected item

 PHA                    \ Store A on the stack so we can preserve it

 TYA                    \ Set Y = Y + 4 mod 8
 CLC                    \
 ADC #4                 \ so the highlight moves to the other side
 AND #7
 TAY

 PLA                    \ Restore A from the stack

 STY selection          \ Update the number of the currently selected item

 JSR AddHighlight       \ Highlight the newly selected item

.tite9

 STA debounce           \ Store the key press in debounce so we can ensure it
                        \ is released before registering another key press

 CMP #&49               \ If RETURN is being pressed, jump to tite20 to return     
 BEQ tite10             \ from the subroutine

 JMP TLL2               \ Loop back up to move/rotate the ship and check again
                        \ for a key press

.tite10

                        \ We now clear the menu area and move the ship away from
                        \ us

 JSR ClearMenu          \ Clear the menu

 LDA INWK+7             \ If z_hi (the ship's distance) is 96, jump to tite11
 CMP #96                \ to return from the subroutine
 BEQ tite11

 INC INWK+7             \ Decrement the ship's distance, to move the ship away
                        \ from us

 JSR MVEIT              \ Move the ship in space according to the orientation
                        \ vectors and the new value in z_hi

 LDA #128               \ Set z_lo = 128, so the closest the ship gets to us is
 STA INWK+6             \ z_hi = 1, z_lo = 128, or 256 + 128 = 384

 ASL A                  \ Set A = 0

 STA INWK               \ Set x_lo = 0, so the ship remains in the screen centre

 LDA #10                \ Set y_lo = 10, so the ship remains slightly above the
 STA INWK+3             \ screen centre

 JSR LL9                \ Call LL9 to display the ship

 DEC MCNT               \ Decrement the main loop counter

 JMP tite10             \ Loop back up to move/rotate the ship

.tite11

 RTS                    \ Return from the subroutine

.tite12

                        \ If we get here then this is an info screen

 STA debounce           \ Store the key press in debounce so we can ensure it
                        \ is released before registering another key press

 CMP #&70               \ If ESCAPE is not being pressed, jump to tite13
 BNE tite13             \ to move on to the next key press

                        \ If we get here then ESCAPE is being pressed

 LDA #0                 \ Set menuType to 0 so we can reshow the main menu
 STA menuType

 BEQ tite10             \ Jump to tite10 to show the ship moving away from us
                        \ and return from the subroutine

.tite13

 CMP #&49               \ If RETURN is being pressed, jump to tite10 to return     
 BEQ tite10             \ from the subroutine

 JMP TLL2               \ Loop back up to move/rotate the ship and check again
                        \ for a key press

\ ******************************************************************************
\
\       Name: ZERO
\       Type: Subroutine
\   Category: Utility routines
\    Summary: Reset the local bubble of universe and ship status
\
\ ******************************************************************************

.ZERO

 LDA #0                 \ Set A = 0 so we can zero the variables

 STA FRIN               \ Zero the ship slot
 STA FRIN+1

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: FX200
\       Type: Subroutine
\   Category: Utility routines
\    Summary: Set the behaviour of the ESCAPE and BREAK keys
\
\ ------------------------------------------------------------------------------
\
\ This is the equivalent of a *FX 200 command, which controls the behaviour of
\ the ESCAPE and BREAK keys.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   Controls the behaviour as follows:
\
\                         * 0 = Enable ESCAPE key
\                               Normal BREAK key action
\
\                         * 1 = Disable ESCAPE key
\                               Normal BREAK key action
\
\                         * 2 = Enable ESCAPE key
\                               Clear memory if the BREAK key is pressed
\
\                         * 3 = Disable ESCAPE key
\                               Clear memory if the BREAK key is pressed
\
\ ******************************************************************************

.FX200

 LDY #0                 \ Call OSBYTE 200 with Y = 0, so the new value is set to
 LDA #200               \ X, and return from the subroutine using a tail call
 JMP OSBYTE

\ ******************************************************************************
\
\       Name: NORM
\       Type: Subroutine
\   Category: Maths (Geometry)
\    Summary: Normalise the three-coordinate vector in XX15
\  Deep dive: Tidying orthonormal vectors
\             Orientation vectors
\
\ ------------------------------------------------------------------------------
\
\ We do this by dividing each of the three coordinates by the length of the
\ vector, which we can calculate using Pythagoras. Once normalised, 96 (&60) is
\ used to represent a value of 1, and 96 with bit 7 set (&E0) is used to
\ represent -1. This enables us to represent fractional values of less than 1
\ using integers.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   XX15                The vector to normalise, with:
\
\                         * The x-coordinate in XX15
\
\                         * The y-coordinate in XX15+1
\
\                         * The z-coordinate in XX15+2
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   XX15                The normalised vector
\
\   Q                   The length of the original XX15 vector
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   NO1                 Contains an RTS
\
\ ******************************************************************************

.NORM

 LDA XX15               \ Fetch the x-coordinate into A

 JSR SQUA               \ Set (A P) = A * A = x^2

 STA R                  \ Set (R Q) = (A P) = x^2
 LDA P
 STA Q

 LDA XX15+1             \ Fetch the y-coordinate into A

 JSR SQUA               \ Set (A P) = A * A = y^2

 STA T                  \ Set (T P) = (A P) = y^2

 LDA P                  \ Set (R Q) = (R Q) + (T P) = x^2 + y^2
 ADC Q                  \
 STA Q                  \ First, doing the low bytes, Q = Q + P

 LDA T                  \ And then the high bytes, R = R + T
 ADC R
 STA R

 LDA XX15+2             \ Fetch the z-coordinate into A

 JSR SQUA               \ Set (A P) = A * A = z^2

 STA T                  \ Set (T P) = (A P) = z^2

 LDA P                  \ Set (R Q) = (R Q) + (T P) = x^2 + y^2 + z^2
 ADC Q                  \
 STA Q                  \ First, doing the low bytes, Q = Q + P

 LDA T                  \ And then the high bytes, R = R + T
 ADC R
 STA R

 JSR LL5                \ We now have the following:
                        \
                        \ (R Q) = x^2 + y^2 + z^2
                        \
                        \ so we can call LL5 to use Pythagoras to get:
                        \
                        \ Q = SQRT(R Q)
                        \   = SQRT(x^2 + y^2 + z^2)
                        \
                        \ So Q now contains the length of the vector (x, y, z),
                        \ and we can normalise the vector by dividing each of
                        \ the coordinates by this value, which we do by calling
                        \ routine TIS2. TIS2 returns the divided figure, using
                        \ 96 to represent 1 and 96 with bit 7 set for -1

 LDA XX15               \ Call TIS2 to divide the x-coordinate in XX15 by Q,
 JSR TIS2               \ with 1 being represented by 96
 STA XX15

 LDA XX15+1             \ Call TIS2 to divide the y-coordinate in XX15+1 by Q,
 JSR TIS2               \ with 1 being represented by 96
 STA XX15+1

 LDA XX15+2             \ Call TIS2 to divide the z-coordinate in XX15+2 by Q,
 JSR TIS2               \ with 1 being represented by 96
 STA XX15+2

.NO1

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: RDKEY
\       Type: Subroutine
\   Category: Keyboard
\    Summary: Scan the keyboard for key presses
\
\ ------------------------------------------------------------------------------
\
\ Scan the keyboard, starting with internal key number 16 ("Q") and working
\ through the set of internal key numbers (see p.142 of the Advanced User Guide
\ for a list of internal key numbers).
\
\ This routine is effectively the same as OSBYTE 122, though the OSBYTE call
\ preserves A, unlike this routine.
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   X                   If a key is being pressed, X contains the internal key
\                       number, otherwise it contains 0
\
\   A                   Contains the same as X
\
\ ******************************************************************************

.RDKEY

 LDX #16                \ Start the scan with internal key number 16 ("Q")

.Rd1

 JSR DKS4               \ Scan the keyboard to see if the key in X is currently
                        \ being pressed, returning the result in A and X

 BMI Rd2                \ Jump to Rd2 if this key is being pressed (in which
                        \ case DKS4 will have returned the key number with bit
                        \ 7 set, which is negative)

 INX                    \ Increment the key number, which was unchanged by the
                        \ above call to DKS4

 BPL Rd1                \ Loop back to test the next key, ending the loop when
                        \ X is negative (i.e. 128)

 TXA                    \ If we get here, nothing is being pressed, so copy X
                        \ into A so that X = A = 128 = %10000000

.Rd2

 EOR #%10000000         \ EOR A with #%10000000 to flip bit 7, so A now contains
                        \ 0 if no key has been pressed, or the internal key
                        \ number if a key has been pressed

 TAX                    \ Copy A into X

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: DKS4
\       Type: Subroutine
\   Category: Keyboard
\    Summary: Scan the keyboard to see if a specific key is being pressed
\  Deep dive: The key logger
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   The internal number of the key to check (see p.142 of
\                       the Advanced User Guide for a list of internal key
\                       numbers)
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   A                   If the key in A is being pressed, A contains the
\                       original argument A, but with bit 7 set (i.e. A + 128).
\                       If the key in A is not being pressed, the value in A is
\                       unchanged
\
\   X                   Contains the same as A
\
\ ******************************************************************************

.DKS4

 LDA #%00000011         \ Set A to %00000011, so it's ready to send to SHEILA
                        \ once interrupts have been disabled

 SEI                    \ Disable interrupts so we can scan the keyboard
                        \ without being hijacked

 STA VIA+&40            \ Set 6522 System VIA output register ORB (SHEILA &40)
                        \ to %00000011 to stop auto scan of keyboard

 LDA #%01111111         \ Set 6522 System VIA data direction register DDRA
 STA VIA+&43            \ (SHEILA &43) to %01111111. This sets the A registers
                        \ (IRA and ORA) so that:
                        \
                        \   * Bits 0-6 of ORA will be sent to the keyboard
                        \
                        \   * Bit 7 of IRA will be read from the keyboard

 STX VIA+&4F            \ Set 6522 System VIA output register ORA (SHEILA &4F)
                        \ to X, the key we want to scan for; bits 0-6 will be
                        \ sent to the keyboard, of which bits 0-3 determine the
                        \ keyboard column, and bits 4-6 the keyboard row

 LDX VIA+&4F            \ Read 6522 System VIA output register IRA (SHEILA &4F)
                        \ into X; bit 7 is the only bit that will have changed.
                        \ If the key is pressed, then bit 7 will be set,
                        \ otherwise it will be clear

 LDA #%00001011         \ Set 6522 System VIA output register ORB (SHEILA &40)
 STA VIA+&40            \ to %00001011 to restart auto scan of keyboard

 CLI                    \ Allow interrupts again

 TXA                    \ Transfer X into A

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: TIDY
\       Type: Subroutine
\   Category: Maths (Geometry)
\    Summary: Orthonormalise the orientation vectors for a ship
\  Deep dive: Tidying orthonormal vectors
\             Orientation vectors
\
\ ------------------------------------------------------------------------------
\
\ This routine orthonormalises the orientation vectors for a ship. This means
\ making the three orientation vectors orthogonal (perpendicular to each other),
\ and normal (so each of the vectors has length 1).
\
\ We do this because we use the small angle approximation to rotate these
\ vectors in space. It is not completely accurate, so the three vectors tend
\ to get stretched over time, so periodically we tidy the vectors with this
\ routine to ensure they remain as orthonormal as possible.
\
\ ******************************************************************************

.TI2

                        \ Called from below with A = 0, X = 0, Y = 4 when
                        \ nosev_x and nosev_y are small, so we assume that
                        \ nosev_z is big

 TYA                    \ A = Y = 4
 LDY #2
 JSR TIS3               \ Call TIS3 with X = 0, Y = 2, A = 4, to set roofv_z =
 STA INWK+20            \ -(nosev_x * roofv_x + nosev_y * roofv_y) / nosev_z

 JMP TI3                \ Jump to TI3 to keep tidying

.TI1

                        \ Called from below with A = 0, Y = 4 when nosev_x is
                        \ small

 TAX                    \ Set X = A = 0

 LDA XX15+1             \ Set A = nosev_y, and if the top two magnitude bits
 AND #%01100000         \ are both clear, jump to TI2 with A = 0, X = 0, Y = 4
 BEQ TI2

 LDA #2                 \ Otherwise nosev_y is big, so set up the index values
                        \ to pass to TIS3

 JSR TIS3               \ Call TIS3 with X = 0, Y = 4, A = 2, to set roofv_y =
 STA INWK+18            \ -(nosev_x * roofv_x + nosev_z * roofv_z) / nosev_y

 JMP TI3                \ Jump to TI3 to keep tidying

.TIDY

 LDA INWK+10            \ Set (XX15, XX15+1, XX15+2) = nosev
 STA XX15
 LDA INWK+12
 STA XX15+1
 LDA INWK+14
 STA XX15+2

 JSR NORM               \ Call NORM to normalise the vector in XX15, i.e. nosev

 LDA XX15               \ Set nosev = (XX15, XX15+1, XX15+2)
 STA INWK+10
 LDA XX15+1
 STA INWK+12
 LDA XX15+2
 STA INWK+14

 LDY #4                 \ Set Y = 4

 LDA XX15               \ Set A = nosev_x, and if the top two magnitude bits
 AND #%01100000         \ are both clear, jump to TI1 with A = 0, Y = 4
 BEQ TI1

 LDX #2                 \ Otherwise nosev_x is big, so set up the index values
 LDA #0                 \ to pass to TIS3

 JSR TIS3               \ Call TIS3 with X = 2, Y = 4, A = 0, to set roofv_x =
 STA INWK+16            \ -(nosev_y * roofv_y + nosev_z * roofv_z) / nosev_x

.TI3

 LDA INWK+16            \ Set (XX15, XX15+1, XX15+2) = roofv
 STA XX15
 LDA INWK+18
 STA XX15+1
 LDA INWK+20
 STA XX15+2

 JSR NORM               \ Call NORM to normalise the vector in XX15, i.e. roofv

 LDA XX15               \ Set roofv = (XX15, XX15+1, XX15+2)
 STA INWK+16
 LDA XX15+1
 STA INWK+18
 LDA XX15+2
 STA INWK+20

 LDA INWK+12            \ Set Q = nosev_y
 STA Q

 LDA INWK+20            \ Set A = roofv_z

 JSR MULT12             \ Set (S R) = Q * A = nosev_y * roofv_z

 LDX INWK+14            \ Set X = nosev_z

 LDA INWK+18            \ Set A = roofv_y

 JSR TIS1               \ Set (A ?) = (-X * A + (S R)) / 96
                        \        = (-nosev_z * roofv_y + nosev_y * roofv_z) / 96
                        \
                        \ This also sets Q = nosev_z

 EOR #%10000000         \ Set sidev_x = -A
 STA INWK+22            \        = (nosev_z * roofv_y - nosev_y * roofv_z) / 96

 LDA INWK+16            \ Set A = roofv_x

 JSR MULT12             \ Set (S R) = Q * A = nosev_z * roofv_x

 LDX INWK+10            \ Set X = nosev_x

 LDA INWK+20            \ Set A = roofv_z

 JSR TIS1               \ Set (A ?) = (-X * A + (S R)) / 96
                        \        = (-nosev_x * roofv_z + nosev_z * roofv_x) / 96
                        \
                        \ This also sets Q = nosev_x

 EOR #%10000000         \ Set sidev_y = -A
 STA INWK+24            \        = (nosev_x * roofv_z - nosev_z * roofv_x) / 96

 LDA INWK+18            \ Set A = roofv_y

 JSR MULT12             \ Set (S R) = Q * A = nosev_x * roofv_y

 LDX INWK+12            \ Set X = nosev_y

 LDA INWK+16            \ Set A = roofv_x

 JSR TIS1               \ Set (A ?) = (-X * A + (S R)) / 96
                        \        = (-nosev_y * roofv_x + nosev_x * roofv_y) / 96

 EOR #%10000000         \ Set sidev_z = -A
 STA INWK+26            \        = (nosev_y * roofv_x - nosev_x * roofv_y) / 96

 LDA #0                 \ Set A = 0 so we can clear the low bytes of the
                        \ orientation vectors

 LDX #14                \ We want to clear the low bytes, so start from sidev_y
                        \ at byte #9+14 (we clear all except sidev_z_lo, though
                        \ I suspect this is in error and that X should be 16)

.TIL1

 STA INWK+9,X           \ Set the low byte in byte #9+X to zero

 DEX                    \ Set X = X - 2 to jump down to the next low byte
 DEX

 BPL TIL1               \ Loop back until we have zeroed all the low bytes

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: TIS2
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate A = A / Q
\  Deep dive: Shift-and-subtract division
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following division, where A is a sign-magnitude number and Q is
\ a positive integer:
\
\   A = A / Q
\
\ The value of A is returned as a sign-magnitude number with 96 representing 1,
\ and the maximum value returned is 1 (i.e. 96). This routine is used when
\ normalising vectors, where we represent fractions using integers, so this
\ gives us an approximation to two decimal places.
\
\ ******************************************************************************

.TIS2

 TAY                    \ Store the argument A in Y

 AND #%01111111         \ Strip the sign bit from the argument, so A = |A|

 CMP Q                  \ If A >= Q then jump to TI4 to return a 1 with the
 BCS TI4                \ correct sign

 LDX #%11111110         \ Set T to have bits 1-7 set, so we can rotate through 7
 STX T                  \ loop iterations, getting a 1 each time, and then
                        \ getting a 0 on the 8th iteration... and we can also
                        \ use T to catch our result bits into bit 0 each time

.TIL2

 ASL A                  \ Shift A to the left

 CMP Q                  \ If A < Q skip the following subtraction
 BCC P%+4

 SBC Q                  \ A >= Q, so set A = A - Q
                        \
                        \ Going into this subtraction we know the C flag is
                        \ set as we passed through the BCC above, and we also
                        \ know that A >= Q, so the C flag will still be set once
                        \ we are done

 ROL T                  \ Rotate the counter in T to the left, and catch the
                        \ result bit into bit 0 (which will be a 0 if we didn't
                        \ do the subtraction, or 1 if we did)

 BCS TIL2               \ If we still have set bits in T, loop back to TIL2 to
                        \ do the next iteration of 7

                        \ We've done the division and now have a result in the
                        \ range 0-255 here, which we need to reduce to the range
                        \ 0-96. We can do that by multiplying the result by 3/8,
                        \ as 256 * 3/8 = 96

 LDA T                  \ Set T = T / 4
 LSR A
 LSR A
 STA T

 LSR A                  \ Set T = T / 8 + T / 4
 ADC T                  \       = 3T / 8
 STA T

 TYA                    \ Fetch the sign bit of the original argument A
 AND #%10000000

 ORA T                  \ Apply the sign bit to T

 RTS                    \ Return from the subroutine

.TI4

 TYA                    \ Fetch the sign bit of the original argument A
 AND #%10000000

 ORA #96                \ Apply the sign bit to 96 (which represents 1)

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: TIS3
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate -(nosev_1 * roofv_1 + nosev_2 * roofv_2) / nosev_3
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following expression:
\
\   A = -(nosev_1 * roofv_1 + nosev_2 * roofv_2) / nosev_3
\
\ where 1, 2 and 3 are x, y, or z, depending on the values of X, Y and A. This
\ routine is called with the following values:
\
\   X = 0, Y = 2, A = 4 ->
\         A = -(nosev_x * roofv_x + nosev_y * roofv_y) / nosev_z
\
\   X = 0, Y = 4, A = 2 ->
\         A = -(nosev_x * roofv_x + nosev_z * roofv_z) / nosev_y
\
\   X = 2, Y = 4, A = 0 ->
\         A = -(nosev_y * roofv_y + nosev_z * roofv_z) / nosev_x
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   X                   Index 1 (0 = x, 2 = y, 4 = z)
\
\   Y                   Index 2 (0 = x, 2 = y, 4 = z)
\
\   A                   Index 3 (0 = x, 2 = y, 4 = z)
\
\ ******************************************************************************

.TIS3

 STA P+2                \ Store P+2 in A for later

 LDA INWK+10,X          \ Set Q = nosev_x_hi (plus X)
 STA Q

 LDA INWK+16,X          \ Set A = roofv_x_hi (plus X)

 JSR MULT12             \ Set (S R) = Q * A
                        \           = nosev_x_hi * roofv_x_hi

 LDX INWK+10,Y          \ Set Q = nosev_x_hi (plus Y)
 STX Q

 LDA INWK+16,Y          \ Set A = roofv_x_hi (plus Y)

 JSR MAD                \ Set (A X) = Q * A + (S R)
                        \           = (nosev_x,X * roofv_x,X) +
                        \             (nosev_x,Y * roofv_x,Y)

 STX P                  \ Store low byte of result in P, so result is now in
                        \ (A P)

 LDY P+2                \ Set Q = roofv_x_hi (plus argument A)
 LDX INWK+10,Y
 STX Q

 EOR #%10000000         \ Flip the sign of A

                        \ Fall through into DIVDT to do:
                        \
                        \   (P+1 A) = (A P) / Q
                        \
                        \     = -((nosev_x,X * roofv_x,X) +
                        \         (nosev_x,Y * roofv_x,Y))
                        \       / nosev_x,A

\ ******************************************************************************
\
\       Name: DVIDT
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (P+1 A) = (A P) / Q
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following integer division between sign-magnitude numbers:
\
\   (P+1 A) = (A P) / Q
\
\ This uses the same shift-and-subtract algorithm as TIS2.
\
\ ******************************************************************************

.DVIDT

 STA P+1                \ Set P+1 = A, so P(1 0) = (A P)

 EOR Q                  \ Set T = the sign bit of A EOR Q, so it's 1 if A and Q
 AND #%10000000         \ have different signs, i.e. it's the sign of the result
 STA T                  \ of A / Q

 LDA #0                 \ Set A = 0 for us to build a result

 LDX #16                \ Set a counter in X to count the 16 bits in P(1 0)

 ASL P                  \ Shift P(1 0) left
 ROL P+1

 ASL Q                  \ Clear the sign bit of Q the C flag at the same time
 LSR Q

.DVL2

 ROL A                  \ Shift A to the left

 CMP Q                  \ If A < Q skip the following subtraction
 BCC P%+4

 SBC Q                  \ Set A = A - Q
                        \
                        \ Going into this subtraction we know the C flag is
                        \ set as we passed through the BCC above, and we also
                        \ know that A >= Q, so the C flag will still be set once
                        \ we are done

 ROL P                  \ Rotate P(1 0) to the left, and catch the result bit
 ROL P+1                \ into the C flag (which will be a 0 if we didn't
                        \ do the subtraction, or 1 if we did)

 DEX                    \ Decrement the loop counter

 BNE DVL2               \ Loop back for the next bit until we have done all 16
                        \ bits of P(1 0)

 LDA P                  \ Set A = P so the low byte is in the result in A

 ORA T                  \ Set A to the correct sign bit that we set in T above

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: SHPPT
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw a distant ship as a point in the middle of the screen
\
\ ******************************************************************************

.SHPPT

 LDA #Y                 \ Set A = the y-coordinate of a dot halfway down the
                        \ screen

 CMP #Y*2-2             \ If the y-coordinate is bigger than the y-coordinate of
 BCS nono               \ the bottom of the screen, jump to nono as the ship's
                        \ dot is off the bottom of the space view. This will
                        \ never happen, but this code is copied from the flight
                        \ code, where A can contain any y-coordinate

 JSR Shpt               \ Call Shpt to draw a horizontal 4-pixel dash for the 
                        \ first row of the dot (i.e. a four-pixel dash)

 LDA #%00001000         \ Set bit 3 of the ship's byte #31 to record that we
 ORA XX1+31             \ have now drawn something on-screen for this ship
 STA XX1+31

 JMP LL155              \ Jump to LL155 to draw any remaining lines that are
                        \ still in the ship line heap and return from the
                        \ subroutine using a tail call

.nono

 LDA #%11110111         \ Clear bit 3 of the ship's byte #31 to record that
 AND XX1+31             \ nothing is being drawn on-screen for this ship
 STA XX1+31

 JMP LL155              \ Jump to LL155 to draw any remaining lines that are
                        \ still in the ship line heap and return from the
                        \ subroutine using a tail call

.Shpt

                        \ This routine draws a horizontal 4-pixel dash, for
                        \ either the top or the bottom of the ship's dot

 STA Y1                 \ Store A in both y-coordinates, as this is a horizontal
 STA Y2                 \ dash at y-coordinate A

 LDA #X                 \ Set A = x-coordinate of the middle of the screen

 STA X1                 \ Store the x-coordinate of the ship dot in X1, as this
                        \ is where the dash starts

 CLC                    \ Set A = screen x-coordinate of the ship dot + 8
 ADC #8

 BCC P%+4               \ If the addition overflowed, set A = 255, the
 LDA #255               \ x-coordinate of the right edge of the screen

 STA X2                 \ Store the x-coordinate of the ship dot in X1, as this
                        \ is where the dash starts

 JMP LSPUT              \ Draw this edge using smooth animation, by first
                        \ drawing the ship's new line and then erasing the
                        \ corresponding old line from the screen, and return
                        \ from the subroutine using a tail call

\ ******************************************************************************
\
\       Name: LL5
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate Q = SQRT(R Q)
\  Deep dive: Calculating square roots
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following square root:
\
\   Q = SQRT(R Q)
\
\ ******************************************************************************

.LL5

 LDY R                  \ Set (Y S) = (R Q)
 LDA Q
 STA S

                        \ So now to calculate Q = SQRT(Y S)

 LDX #0                 \ Set X = 0, to hold the remainder

 STX Q                  \ Set Q = 0, to hold the result

 LDA #8                 \ Set T = 8, to use as a loop counter
 STA T

.LL6

 CPX Q                  \ If X < Q, jump to LL7
 BCC LL7

 BNE LL8                \ If X > Q, jump to LL8

 CPY #64                \ If Y < 64, jump to LL7 with the C flag clear,
 BCC LL7                \ otherwise fall through into LL8 with the C flag set

.LL8

 TYA                    \ Set Y = Y - 64
 SBC #64                \
 TAY                    \ This subtraction will work as we know C is set from
                        \ the BCC above, and the result will not underflow as we
                        \ already checked that Y >= 64, so the C flag is also
                        \ set for the next subtraction

 TXA                    \ Set X = X - Q
 SBC Q
 TAX

.LL7

 ROL Q                  \ Shift the result in Q to the left, shifting the C flag
                        \ into bit 0 and bit 7 into the C flag

 ASL S                  \ Shift the dividend in (Y S) to the left, inserting
 TYA                    \ bit 7 from above into bit 0
 ROL A
 TAY

 TXA                    \ Shift the remainder in X to the left
 ROL A
 TAX

 ASL S                  \ Shift the dividend in (Y S) to the left
 TYA
 ROL A
 TAY

 TXA                    \ Shift the remainder in X to the left
 ROL A
 TAX

 DEC T                  \ Decrement the loop counter

 BNE LL6                \ Loop back to LL6 until we have done 8 loops

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL28
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate R = 256 * A / Q
\  Deep dive: Shift-and-subtract division
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following, where A < Q:
\
\   R = 256 * A / Q
\
\ This is a sister routine to LL61, which does the division when A >= Q.
\
\ If A >= Q then 255 is returned and the C flag is set to indicate an overflow
\ (the C flag is clear if the division was a success).
\
\ The result is returned in one byte as the result of the division multiplied
\ by 256, so we can return fractional results using integers.
\
\ This routine uses the same shift-and-subtract algorithm that's documented in
\ TIS2, but it leaves the fractional result in the integer range 0-255.
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   C flag              Set if the answer is too big for one byte, clear if the
\                       division was a success
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   LL28+4              Skips the A >= Q check and always returns with C flag
\                       cleared, so this can be called if we know the division
\                       will work
\
\   LL31                Skips the A >= Q check and does not set the R counter,
\                       so this can be used for jumping straight into the
\                       division loop if R is already set to 254 and we know the
\                       division will work
\
\ ******************************************************************************

.LL28

 CMP Q                  \ If A >= Q, then the answer will not fit in one byte,
 BCS LL2                \ so jump to LL2 to return 255

 LDX #%11111110         \ Set R to have bits 1-7 set, so we can rotate through 7
 STX R                  \ loop iterations, getting a 1 each time, and then
                        \ getting a 0 on the 8th iteration... and we can also
                        \ use R to catch our result bits into bit 0 each time

.LL31

 ASL A                  \ Shift A to the left

 BCS LL29               \ If bit 7 of A was set, then jump straight to the
                        \ subtraction

 CMP Q                  \ If A < Q, skip the following subtraction
 BCC P%+4

 SBC Q                  \ A >= Q, so set A = A - Q

 ROL R                  \ Rotate the counter in R to the left, and catch the
                        \ result bit into bit 0 (which will be a 0 if we didn't
                        \ do the subtraction, or 1 if we did)

 BCS LL31               \ If we still have set bits in R, loop back to LL31 to
                        \ do the next iteration of 7

 RTS                    \ R left with remainder of division

.LL29

 SBC Q                  \ A >= Q, so set A = A - Q

 SEC                    \ Set the C flag to rotate into the result in R

 ROL R                  \ Rotate the counter in R to the left, and catch the
                        \ result bit into bit 0 (which will be a 0 if we didn't
                        \ do the subtraction, or 1 if we did)

 BCS LL31               \ If we still have set bits in R, loop back to LL31 to
                        \ do the next iteration of 7

 RTS                    \ Return from the subroutine with R containing the
                        \ remainder of the division

.LL2

 LDA #255               \ The division is very close to 1, so return the closest
 STA R                  \ possible answer to 256, i.e. R = 255

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL38
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (S A) = (S R) + (A Q)
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following between sign-magnitude numbers:
\
\   (S A) = (S R) + (A Q)
\
\ where the sign bytes only contain the sign bits, not magnitudes.
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   C flag              Set if the addition overflowed, clear otherwise
\
\ ******************************************************************************

.LL38

 EOR S                  \ If the sign of A * S is negative, skip to LL35, as
 BMI LL39               \ A and S have different signs so we need to subtract

 LDA Q                  \ Otherwise set A = R + Q, which is the result we need,
 CLC                    \ as S already contains the correct sign
 ADC R

 RTS                    \ Return from the subroutine

.LL39

 LDA R                  \ Set A = R - Q
 SEC
 SBC Q

 BCC P%+4               \ If the subtraction underflowed, skip the next two
                        \ instructions so we can negate the result

 CLC                    \ Otherwise the result is correct, and S contains the
                        \ correct sign of the result as R is the dominant side
                        \ of the subtraction, so clear the C flag

 RTS                    \ And return from the subroutine

                        \ If we get here we need to negate both the result and
                        \ the sign in S, as both are the wrong sign

 PHA                    \ Store the result of the subtraction on the stack

 LDA S                  \ Flip the sign of S
 EOR #%10000000
 STA S

 PLA                    \ Restore the subtraction result into A

 EOR #%11111111         \ Negate the result in A using two's complement, i.e.
 ADC #1                 \ set A = ~A + 1

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL51
\       Type: Subroutine
\   Category: Maths (Geometry)
\    Summary: Calculate the dot product of XX15 and XX16
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following dot products:
\
\   XX12(1 0) = XX15(5 0) . XX16(5 0)
\   XX12(3 2) = XX15(5 0) . XX16(11 6)
\   XX12(5 4) = XX15(5 0) . XX16(12 17)
\
\ storing the results as sign-magnitude numbers in XX12 through XX12+5.
\
\ When called from part 5 of LL9, XX12 contains the vector [x y z] to the ship
\ we're drawing, and XX16 contains the orientation vectors, so it returns:
\
\   [ x ]   [ sidev_x ]         [ x ]   [ roofv_x ]         [ x ]   [ nosev_x ]
\   [ y ] . [ sidev_y ]         [ y ] . [ roofv_y ]         [ y ] . [ nosev_y ]
\   [ z ]   [ sidev_z ]         [ z ]   [ roofv_z ]         [ z ]   [ nosev_z ]
\
\ When called from part 6 of LL9, XX12 contains the vector [x y z] of the vertex
\ we're analysing, and XX16 contains the transposed orientation vectors with
\ each of them containing the x, y and z elements of the original vectors, so it
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   [ x ]   [ sidev_x ]         [ x ]   [ sidev_y ]         [ x ]   [ sidev_z ]
\   [ y ] . [ roofv_x ]         [ y ] . [ roofv_y ]         [ y ] . [ roofv_z ]
\   [ z ]   [ nosev_x ]         [ z ]   [ nosev_y ]         [ z ]   [ nosev_z ]
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   XX15(1 0)           The ship (or vertex)'s x-coordinate as (x_sign x_lo)
\
\   XX15(3 2)           The ship (or vertex)'s y-coordinate as (y_sign y_lo)
\
\   XX15(5 4)           The ship (or vertex)'s z-coordinate as (z_sign z_lo)
\
\   XX16 to XX16+5      The scaled sidev (or _x) vector, with:
\
\                         * x, y, z magnitudes in XX16, XX16+2, XX16+4
\
\                         * x, y, z signs in XX16+1, XX16+3, XX16+5
\
\   XX16+6 to XX16+11   The scaled roofv (or _y) vector, with:
\
\                         * x, y, z magnitudes in XX16+6, XX16+8, XX16+10
\
\                         * x, y, z signs in XX16+7, XX16+9, XX16+11
\
\   XX16+12 to XX16+17  The scaled nosev (or _z) vector, with:
\
\                         * x, y, z magnitudes in XX16+12, XX16+14, XX16+16
\
\                         * x, y, z signs in XX16+13, XX16+15, XX16+17
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   XX12(1 0)           The dot product of [x y z] vector with the sidev (or _x)
\                       vector, with the sign in XX12+1 and magnitude in XX12
\
\   XX12(3 2)           The dot product of [x y z] vector with the roofv (or _y)
\                       vector, with the sign in XX12+3 and magnitude in XX12+2
\
\   XX12(5 4)           The dot product of [x y z] vector with the nosev (or _z)
\                       vector, with the sign in XX12+5 and magnitude in XX12+4
\
\ ******************************************************************************

.LL51

 LDX #0                 \ Set X = 0, which will contain the offset of the vector
                        \ to use in the calculation, increasing by 6 for each
                        \ new vector

 LDY #0                 \ Set Y = 0, which will contain the offset of the
                        \ result bytes in XX12, increasing by 2 for each new
                        \ result

.ll51

 LDA XX15               \ Set Q = x_lo
 STA Q

 LDA XX16,X             \ Set A = |sidev_x|

 JSR FMLTU              \ Set T = A * Q / 256
 STA T                  \       = |sidev_x| * x_lo / 256

 LDA XX15+1             \ Set S to the sign of x_sign * sidev_x
 EOR XX16+1,X
 STA S

 LDA XX15+2             \ Set Q = y_lo
 STA Q

 LDA XX16+2,X           \ Set A = |sidev_y|

 JSR FMLTU              \ Set Q = A * Q / 256
 STA Q                  \       = |sidev_y| * y_lo / 256

 LDA T                  \ Set R = T
 STA R                  \       = |sidev_x| * x_lo / 256

 LDA XX15+3             \ Set A to the sign of y_sign * sidev_y
 EOR XX16+3,X

 JSR LL38               \ Set (S T) = (S R) + (A Q)
 STA T                  \           = |sidev_x| * x_lo + |sidev_y| * y_lo

 LDA XX15+4             \ Set Q = z_lo
 STA Q

 LDA XX16+4,X           \ Set A = |sidev_z|

 JSR FMLTU              \ Set Q = A * Q / 256
 STA Q                  \       = |sidev_z| * z_lo / 256

 LDA T                  \ Set R = T
 STA R                  \       = |sidev_x| * x_lo + |sidev_y| * y_lo

 LDA XX15+5             \ Set A to the sign of z_sign * sidev_z
 EOR XX16+5,X

 JSR LL38               \ Set (S A) = (S R) + (A Q)
                        \           = |sidev_x| * x_lo + |sidev_y| * y_lo
                        \             + |sidev_z| * z_lo

 STA XX12,Y             \ Store the result in XX12+Y(1 0)
 LDA S
 STA XX12+1,Y

 INY                    \ Set Y = Y + 2
 INY

 TXA                    \ Set X = X + 6
 CLC
 ADC #6
 TAX

 CMP #17                \ If X < 17, loop back to ll51 for the next vector
 BCC ll51

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL9 (Part 1 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Check if ship is exploding, check if ship is in front
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This routine draws the current ship on the screen. This part checks to see if
\ the ship is exploding, or if it should start exploding, and if it does it sets
\ things up accordingly.
\
\ It also does some basic checks to see if we can see the ship, and if not it
\ removes it from the screen.
\
\ In this code, XX1 is used to point to the current ship's data block at INWK
\ (the two labels are interchangeable).
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   XX1                 XX1 shares its location with INWK, which contains the
\                       zero-page copy of the data block for this ship from the
\                       K% workspace
\
\   INF                 The address of the data block for this ship in workspace
\                       K%
\
\   XX19(1 0)           XX19(1 0) shares its location with INWK(34 33), which
\                       contains the ship line heap address pointer
\
\   XX0                 The address of the blueprint for this ship
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   EE51                Remove the current ship from the screen, called from
\                       SHPPT before drawing the ship as a point
\
\ ******************************************************************************

.LL9

 LDA #31                \ Set XX4 = 31 to store the ship's distance for later
 STA XX4                \ comparison with the visibility distance. We will
                        \ update this value below with the actual ship's
                        \ distance if it turns out to be visible on-screen

                        \ We now set things up for smooth ship plotting, by
                        \ setting the following:
                        \
                        \   LSNUM = offset to the first coordinate in the ship's
                        \           line heap
                        \
                        \   LSNUM2 = the number of bytes in the heap for the
                        \            ship that's currently on-screen (or 0 if
                        \            there is no ship currently on-screen)

 LDY #1                 \ Set LSNUM = 1, the offset of the first set of line
 STY LSNUM              \ coordinates in the ship line heap

 DEY                    \ Decrement Y to 0

 LDA #%00001000         \ If bit 3 of the ship's byte #31 is set, then the ship
 BIT INWK+31            \ is currently being drawn on-screen, so skip the
 BNE P%+5               \ following two instructions

 LDA #0                 \ The ship is not being drawn on screen, so set A = 0
                        \ so that LSNUM2 gets set to 0 below (as there are no
                        \ existing coordinates on the ship line heap for this
                        \ ship)
 
 EQUB &2C               \ Skip the next instruction by turning it into
                        \ &2C &B1 &BD, or BIT &BDB1 which does nothing apart
                        \ from affect the flags

 LDA (XX19),Y           \ Set LSNUM2 to the first byte of the ship's line heap,
 STA LSNUM2             \ which contains the number of bytes in the heap

 LDA #%00100000         \ If bit 5 of the ship's byte #31 is set, then the ship
 BIT XX1+31             \ is currently exploding, so jump down to EE28
 BNE EE28

 BPL EE28               \ If bit 7 of the ship's byte #31 is clear then the ship
                        \ has not just been killed, so jump down to EE28

                        \ Otherwise bit 5 is clear and bit 7 is set, so the ship
                        \ is not yet exploding but it has been killed, so we
                        \ need to start an explosion

 ORA XX1+31             \ Clear bits 6 and 7 of the ship's byte #31, to stop the
 AND #%00111111         \ ship from firing its laser and to mark it as no longer
 STA XX1+31             \ having just been killed

 LDA #0                 \ Set the ship's acceleration in byte #31 to 0, updating
 LDY #28                \ the byte in the workspace K% data block so we don't
 STA (INF),Y            \ have to copy it back from INWK later

 LDY #30                \ Set the ship's pitch counter in byte #30 to 0, to stop
 STA (INF),Y            \ the ship from pitching

 JSR EE51               \ Call EE51 to remove the ship from the screen

                        \ We now need to set up a new explosion cloud. We
                        \ initialise it with a size of 18 (which gets increased
                        \ by 4 every time the cloud gets redrawn), and the
                        \ explosion count (i.e. the number of particles in the
                        \ explosion), which go into bytes 1 and 2 of the ship
                        \ line heap. See DOEXP for more details of explosion
                        \ clouds

 LDY #1                 \ Set byte #1 of the ship line heap to 18, the initial
 LDA #18                \ size of the explosion cloud
 STA (XX19),Y

 LDY #7                 \ Fetch byte #7 from the ship's blueprint, which
 LDA (XX0),Y            \ determines the explosion count (i.e. the number of
 LDY #2                 \ vertices used as origins for explosion clouds), and
 STA (XX19),Y           \ store it in byte #2 of the ship line heap

                        \ The following loop sets bytes 3-6 of the of the ship
                        \ line heap to random numbers

.EE55

 INY                    \ Increment Y (so the loop starts at 3)

\JSR DORND              \ Set A and X to random numbers

 STA (XX19),Y           \ Store A in the Y-th byte of the ship line heap

 CPY #6                 \ Loop back until we have randomised the 6th byte
 BNE EE55

.EE28

 LDA XX1+8              \ Set A = z_sign

.EE49

 BPL LL10               \ If A is positive, i.e. the ship is in front of us,
                        \ jump down to LL10

.LL14

                        \ The following removes the ship from the screen by
                        \ redrawing it (or, if it is exploding, by redrawing the
                        \ explosion cloud). We call it when the ship is no
                        \ longer on-screen, is too far away to be fully drawn,
                        \ and so on

\LDA XX1+31             \ If bit 5 of the ship's byte #31 is clear, then the
\AND #%00100000         \ ship is not currently exploding, so jump down to EE51
\BEQ EE51               \ to redraw its wireframe

\LDA XX1+31             \ The ship is exploding, so clear bit 3 of the ship's
\AND #%11110111         \ byte #31 to denote that the ship is no longer being
\STA XX1+31             \ drawn on-screen

\JMP DOEXP              \ Jump to DOEXP to return from the subroutine using a
                        \ tail call, as in the docked code DOEXP just contains
                        \ an RTS

.EE51

 LDA #%00001000         \ If bit 3 of the ship's byte #31 is clear, then there
 BIT XX1+31             \ is already nothing being shown for this ship, so
 BEQ LL10-1             \ return from the subroutine (as LL10-1 contains an RTS)

 EOR XX1+31             \ Otherwise flip bit 3 of byte #31 and store it (which
 STA XX1+31             \ clears bit 3 as we know it was set before the EOR), so
                        \ this sets this ship as no longer being drawn on-screen

 JMP LL155              \ Jump to LL155 to draw the ship, which removes it from
                        \ the screen, returning from the subroutine using a
                        \ tail call

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL9 (Part 2 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Check if ship is in field of view, close enough to draw
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This part checks whether the ship is in our field of view, and whether it is
\ close enough to be fully drawn (if not, we jump to SHPPT to draw it as a dot).
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   LL10-1              Contains an RTS
\
\ ******************************************************************************

.LL10

 LDA XX1+7              \ Set A = z_hi

 CMP #192               \ If A >= 192 then the ship is a long way away, so jump
 BCS LL14               \ to LL14 to remove the ship from the screen

 LDA XX1                \ If x_lo >= z_lo, set the C flag, otherwise clear it
 CMP XX1+6

 LDA XX1+1              \ Set A = x_hi - z_hi using the carry from the low
 SBC XX1+7              \ bytes, which sets the C flag as if we had done a full
                        \ two-byte subtraction (x_hi x_lo) - (z_hi z_lo)

 BCS LL14               \ If the C flag is set then x >= z, so the ship is
                        \ further to the side than it is in front of us, so it's
                        \ outside our viewing angle of 45 degrees, and we jump
                        \ to LL14 to remove it from the screen

 LDA XX1+3              \ If y_lo >= z_lo, set the C flag, otherwise clear it
 CMP XX1+6

 LDA XX1+4              \ Set A = y_hi - z_hi using the carry from the low
 SBC XX1+7              \ bytes, which sets the C flag as if we had done a full
                        \ two-byte subtraction (y_hi y_lo) - (z_hi z_lo)

 BCS LL14               \ If the C flag is set then y >= z, so the ship is
                        \ further above us than it is in front of us, so it's
                        \ outside our viewing angle of 45 degrees, and we jump
                        \ to LL14 to remove it from the screen

 LDY #6                 \ Fetch byte #6 from the ship's blueprint into X, which
 LDA (XX0),Y            \ is the number * 4 of the vertex used for the ship's
 TAX                    \ laser

 LDA #255               \ Set bytes X and X+1 of the XX3 heap to 255. We're
 STA XX3,X              \ going to use XX3 to store the screen coordinates of
 STA XX3+1,X            \ all the visible vertices of this ship, so setting the
                        \ laser vertex to 255 means that if we don't update this
                        \ vertex with its screen coordinates in parts 6 and 7,
                        \ this vertex's entry in the XX3 heap will still be 255,
                        \ which we can check in part 9 to see if the laser
                        \ vertex is visible (and therefore whether we should
                        \ draw laser lines if the ship is firing on us)

 LDA XX1+6              \ Set (A T) = (z_hi z_lo)
 STA T
 LDA XX1+7

 LSR A                  \ Set (A T) = (A T) / 8
 ROR T
 LSR A
 ROR T
 LSR A
 ROR T

 LSR A                  \ If A >> 4 is non-zero, i.e. z_hi >= 16, jump to LL13
 BNE LL13               \ as the ship is possibly far away enough to be shown as
                        \ a dot

 LDA T                  \ Otherwise the C flag contains the previous bit 0 of A,
 ROR A                  \ which could have been set, so rotate A right four
 LSR A                  \ times so it's in the form %000xxxxx, i.e. z_hi reduced
 LSR A                  \ to a maximum value of 31
 LSR A

 STA XX4                \ Store A in XX4, which is now the distance of the ship
                        \ we can use for visibility testing

 BPL LL17               \ Jump down to LL17 (this BPL is effectively a JMP as we
                        \ know bit 7 of A is definitely clear)

.LL13

                        \ If we get here then the ship is possibly far enough
                        \ away to be shown as a dot

 LDY #13                \ Fetch byte #13 from the ship's blueprint, which gives
 LDA (XX0),Y            \ the ship's visibility distance, beyond which we show
                        \ the ship as a dot

 CMP XX1+7              \ If z_hi <= the visibility distance, skip to LL17 to
 BCS LL17               \ draw the ship fully, rather than as a dot, as it is
                        \ closer than the visibility distance

 LDA #%00100000         \ If bit 5 of the ship's byte #31 is set, then the
 AND XX1+31             \ ship is currently exploding, so skip to LL17 to draw
 BNE LL17               \ the ship's explosion cloud

 JMP SHPPT              \ Otherwise jump to SHPPT to draw the ship as a dot,
                        \ returning from the subroutine using a tail call

\ ******************************************************************************
\
\       Name: LL9 (Part 3 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Set up orientation vector, ship coordinate variables
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This part sets up the following variable blocks:
\
\   * XX16 contains the orientation vectors, divided to normalise them
\
\   * XX18 contains the ship's x, y and z coordinates in space
\
\ ******************************************************************************

.LL17

 LDX #5                 \ First we copy the three orientation vectors into XX16,
                        \ so set up a counter in X for the 6 bytes in each
                        \ vector

.LL15

 LDA XX1+21,X           \ Copy the X-th byte of sidev to the X-th byte of XX16
 STA XX16,X

 LDA XX1+15,X           \ Copy the X-th byte of roofv to XX16+6 to the X-th byte
 STA XX16+6,X           \ of XX16+6

 LDA XX1+9,X            \ Copy the X-th byte of nosev to XX16+12 to the X-th
 STA XX16+12,X          \ byte of XX16+12

 DEX                    \ Decrement the counter

 BPL LL15               \ Loop back to copy the next byte of each vector, until
                        \ we have the following:
                        \
                        \   * XX16(1 0) = sidev_x
                        \   * XX16(3 2) = sidev_y
                        \   * XX16(5 4) = sidev_z
                        \
                        \   * XX16(7 6) = roofv_x
                        \   * XX16(9 8) = roofv_y
                        \   * XX16(11 10) = roofv_z
                        \
                        \   * XX16(13 12) = nosev_x
                        \   * XX16(15 14) = nosev_y
                        \   * XX16(17 16) = nosev_z

 LDA #197               \ Set Q = 197
 STA Q

 LDY #16                \ Set Y to be a counter that counts down by 2 each time,
                        \ starting with 16, then 14, 12 and so on. We use this
                        \ to work through each of the coordinates in each of the
                        \ orientation vectors

.LL21

 LDA XX16,Y             \ Set A = the low byte of the vector coordinate, e.g.
                        \ nosev_z_lo when Y = 16

 ASL A                  \ Shift bit 7 into the C flag

 LDA XX16+1,Y           \ Set A = the high byte of the vector coordinate, e.g.
                        \ nosev_z_hi when Y = 16

 ROL A                  \ Rotate A left, incorporating the C flag, so A now
                        \ contains the original high byte, doubled, and without
                        \ a sign bit, e.g. A = |nosev_z_hi| * 2

 JSR LL28               \ Call LL28 to calculate:
                        \
                        \   R = 256 * A / Q
                        \
                        \ so, for nosev, this would be:
                        \
                        \   R = 256 * |nosev_z_hi| * 2 / 197
                        \     = 2.6 * |nosev_z_hi|

 LDA R                  \ Store R in the low byte's location, so we can keep the
 STA XX16,Y             \ old, unscaled high byte intact for the sign

 DEY                    \ Decrement the loop counter twice
 DEY

 BPL LL21               \ Loop back for the next vector coordinate until we have
                        \ divided them all

                        \ By this point, the vectors have been turned into
                        \ scaled magnitudes, so we have the following:
                        \
                        \   * XX16   = scaled |sidev_x|
                        \   * XX16+2 = scaled |sidev_y|
                        \   * XX16+4 = scaled |sidev_z|
                        \
                        \   * XX16+6  = scaled |roofv_x|
                        \   * XX16+8  = scaled |roofv_y|
                        \   * XX16+10 = scaled |roofv_z|
                        \
                        \   * XX16+12 = scaled |nosev_x|
                        \   * XX16+14 = scaled |nosev_y|
                        \   * XX16+16 = scaled |nosev_z|

 LDX #8                 \ Next we copy the ship's coordinates into XX18, so set
                        \ up a counter in X for 9 bytes

.ll91

 LDA XX1,X              \ Copy the X-th byte from XX1 to XX18
 STA XX18,X

 DEX                    \ Decrement the loop counter

 BPL ll91               \ Loop back for the next byte until we have copied all
                        \ three coordinates

                        \ So we now have the following:
                        \
                        \   * XX18(2 1 0) = (x_sign x_hi x_lo)
                        \
                        \   * XX18(5 4 3) = (y_sign y_hi y_lo)
                        \
                        \   * XX18(8 7 6) = (z_sign z_hi z_lo)

 LDA #255               \ Set the 15th byte of XX2 to 255, so that face 15 is
 STA XX2+15             \ always visible. No ship definitions actually have this
                        \ number of faces, but this allows us to force a vertex
                        \ to always be visible by associating it with face 15
                        \ (see the ship blueprints for the Cobra Mk III at
                        \ SHIP_COBRA_MK_3 and the asteroid at SHIP_ASTEROID for
                        \ examples of vertices that are associated with face 15)

 LDY #12                \ Set Y = 12 to point to the ship blueprint byte #12,

 LDA XX1+31             \ If bit 5 of the ship's byte #31 is clear, then the
 AND #%00100000         \ ship is not currently exploding, so jump down to EE29
 BEQ EE29               \ to skip the following

                        \ Otherwise we fall through to set up the visibility
                        \ block for an exploding ship

\ ******************************************************************************
\
\       Name: LL9 (Part 4 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Set visibility for exploding ship (all faces visible)
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This part sets up the visibility block in XX2 for a ship that is exploding.
\
\ The XX2 block consists of one byte for each face in the ship's blueprint,
\ which holds the visibility of that face. Because the ship is exploding, we
\ want to set all the faces to be visible. A value of 255 in the visibility
\ table means the face is visible, so the following code sets each face to 255
\ and then skips over the face visibility calculations that we would apply to a
\ non-exploding ship.
\
\ ******************************************************************************

 LDA (XX0),Y            \ Fetch byte #12 of the ship's blueprint, which contains
                        \ the number of faces * 4

 LSR A                  \ Set X = A / 4
 LSR A                  \       = the number of faces
 TAX

 LDA #255               \ Set A = 255

.EE30

 STA XX2,X              \ Set the X-th byte of XX2 to 255

 DEX                    \ Decrement the loop counter

 BPL EE30               \ Loop back for the next byte until there is one byte
                        \ set to 255 for each face

 INX                    \ Set XX4 = 0 for the distance value we use to test
 STX XX4                \ for visibility, so we always shows everything

.LL41

 JMP LL42               \ Jump to LL42 to skip the face visibility calculations
                        \ as we don't need to do them now we've set up the XX2
                        \ block for the explosion

\ ******************************************************************************
\
\       Name: LL9 (Part 5 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Calculate the visibility of each of the ship's faces
\  Deep dive: Drawing ships
\             Back-face culling
\
\ ******************************************************************************

.EE29

 LDA (XX0),Y            \ We set Y to 12 above before jumping down to EE29, so
                        \ this fetches byte #12 of the ship's blueprint, which
                        \ contains the number of faces * 4

 BEQ LL41               \ If there are no faces in this ship, jump to LL42 (via
                        \ LL41) to skip the face visibility calculations

 STA XX20               \ Set A = the number of faces * 4

 LDY #18                \ Fetch byte #18 of the ship's blueprint, which contains
 LDA (XX0),Y            \ the factor by which we scale the face normals, into X
 TAX

 LDA XX18+7             \ Set A = z_hi

.LL90

 TAY                    \ Set Y = z_hi

 BEQ LL91               \ If z_hi = 0 then jump to LL91

                        \ The following is a loop that jumps back to LL90+3,
                        \ i.e. here. LL90 is only used for this loop, so it's a
                        \ bit of a strange use of the label here

 INX                    \ Increment the scale factor in X

 LSR XX18+4             \ Divide (y_hi y_lo) by 2
 ROR XX18+3

 LSR XX18+1             \ Divide (x_hi x_lo) by 2
 ROR XX18

 LSR A                  \ Divide (z_hi z_lo) by 2 (as A contains z_hi)
 ROR XX18+6

 TAY                    \ Set Y = z_hi

 BNE LL90+3             \ If Y is non-zero, loop back to LL90+3 to divide the
                        \ three coordinates until z_hi is 0

.LL91

                        \ By this point z_hi is 0 and X contains the number of
                        \ right shifts we had to do, plus the scale factor from
                        \ the blueprint

 STX XX17               \ Store the updated scale factor in XX17

 LDA XX18+8             \ Set XX15+5 = z_sign
 STA XX15+5

 LDA XX18               \ Set XX15(1 0) = (x_sign x_lo)
 STA XX15
 LDA XX18+2
 STA XX15+1

 LDA XX18+3             \ Set XX15(3 2) = (y_sign y_lo)
 STA XX15+2
 LDA XX18+5
 STA XX15+3

 LDA XX18+6             \ Set XX15+4 = z_lo, so now XX15(5 4) = (z_sign z_lo)
 STA XX15+4

 JSR LL51               \ Call LL51 to set XX12 to the dot products of XX15 and
                        \ XX16, which we'll call dot_sidev, dot_roofv and
                        \ dot_nosev:
                        \
                        \   XX12(1 0) = [x y z] . sidev
                        \             = (dot_sidev_sign dot_sidev_lo)
                        \             = dot_sidev
                        \
                        \   XX12(3 2) = [x y z] . roofv
                        \             = (dot_roofv_sign dot_roofv_lo)
                        \             = dot_roofv
                        \
                        \   XX12(5 4) = [x y z] . nosev
                        \             = (dot_nosev_sign dot_nosev_lo)
                        \             = dot_nosev

 LDA XX12               \ Set XX18(2 0) = dot_sidev
 STA XX18
 LDA XX12+1
 STA XX18+2

 LDA XX12+2             \ Set XX18(5 3) = dot_roofv
 STA XX18+3
 LDA XX12+3
 STA XX18+5

 LDA XX12+4             \ Set XX18(8 6) = dot_nosev
 STA XX18+6
 LDA XX12+5
 STA XX18+8

 LDY #4                 \ Fetch byte #4 of the ship's blueprint, which contains
 LDA (XX0),Y            \ the low byte of the offset to the faces data

 CLC                    \ Set V = low byte faces offset + XX0
 ADC XX0
 STA V

 LDY #17                \ Fetch byte #17 of the ship's blueprint, which contains
 LDA (XX0),Y            \ the high byte of the offset to the faces data

 ADC XX0+1              \ Set V+1 = high byte faces offset + XX0+1
 STA V+1                \
                        \ So V(1 0) now points to the start of the faces data
                        \ for this ship

 LDY #0                 \ We're now going to loop through all the faces for this
                        \ ship, so set a counter in Y, starting from 0, which we
                        \ will increment by 4 each loop to step through the
                        \ four bytes of data for each face

.LL86

 LDA (V),Y              \ Fetch byte #0 for this face into A, so:
                        \
                        \   A = %xyz vvvvv, where:
                        \
                        \     * Bits 0-4 = visibility distance, beyond which the
                        \       face is always shown
                        \
                        \     * Bits 7-5 = the sign bits of normal_x, normal_y
                        \       and normal_z

 STA XX12+1             \ Store byte #0 in XX12+1, so XX12+1 now has the sign of
                        \ normal_x

 AND #%00011111         \ Extract bits 0-4 to give the visibility distance

 CMP XX4                \ If XX4 <= the visibility distance, where XX4 contains
 BCS LL87               \ the ship's z-distance reduced to 0-31 (which we set in
                        \ part 2), skip to LL87 as this face is close enough
                        \ that we have to test its visibility using the face
                        \ normals

                        \ Otherwise this face is within range and is therefore
                        \ always shown

 TYA                    \ Set X = Y / 4
 LSR A                  \       = the number of this face * 4 /4
 LSR A                  \       = the number of this face
 TAX

 LDA #255               \ Set the X-th byte of XX2 to 255 to denote that this
 STA XX2,X              \ face is visible

 TYA                    \ Set Y = Y + 4 to point to the next face
 ADC #4
 TAY

 JMP LL88               \ Jump down to LL88 to skip the following, as we don't
                        \ need to test the face normals

.LL87

 LDA XX12+1             \ Fetch byte #0 for this face into A

 ASL A                  \ Shift A left and store it, so XX12+3 now has the sign
 STA XX12+3             \ of normal_y

 ASL A                  \ Shift A left and store it, so XX12+5 now has the sign
 STA XX12+5             \ of normal_z

 INY                    \ Increment Y to point to byte #1

 LDA (V),Y              \ Fetch byte #1 for this face and store in XX12, so
 STA XX12               \ XX12 = normal_x

 INY                    \ Increment Y to point to byte #2

 LDA (V),Y              \ Fetch byte #2 for this face and store in XX12+2, so
 STA XX12+2             \ XX12+2 = normal_y

 INY                    \ Increment Y to point to byte #3

 LDA (V),Y              \ Fetch byte #3 for this face and store in XX12+4, so
 STA XX12+4             \ XX12+4 = normal_z

                        \ So we now have:
                        \
                        \   XX12(1 0) = (normal_x_sign normal_x)
                        \
                        \   XX12(3 2) = (normal_y_sign normal_y)
                        \
                        \   XX12(5 4) = (normal_z_sign normal_z)

 LDX XX17               \ If XX17 < 4 then jump to LL92, otherwise we stored a
 CPX #4                 \ larger scale factor above
 BCC LL92

.LL143

 LDA XX18               \ Set XX15(1 0) = XX18(2 0)
 STA XX15               \               = dot_sidev
 LDA XX18+2
 STA XX15+1

 LDA XX18+3             \ Set XX15(3 2) = XX18(5 3)
 STA XX15+2             \               = dot_roofv
 LDA XX18+5
 STA XX15+3

 LDA XX18+6             \ Set XX15(5 4) = XX18(8 6)
 STA XX15+4             \               = dot_nosev
 LDA XX18+8
 STA XX15+5

 JMP LL89               \ Jump down to LL89

.ovflw

                        \ If we get here then the addition below overflowed, so
                        \ we halve the dot products and normal vector

 LSR XX18               \ Divide dot_sidev_lo by 2, so dot_sidev = dot_sidev / 2

 LSR XX18+6             \ Divide dot_nosev_lo by 2, so dot_nosev = dot_nosev / 2

 LSR XX18+3             \ Divide dot_roofv_lo by 2, so dot_roofv = dot_roofv / 2

 LDX #1                 \ Set X = 1 so when we fall through into LL92, we divide
                        \ the normal vector by 2 as well

.LL92

                        \ We jump here from above with the scale factor in X,
                        \ and now we apply it by scaling the normal vector down
                        \ by a factor of 2^X (i.e. divide by 2^X)

 LDA XX12               \ Set XX15 = normal_x
 STA XX15

 LDA XX12+2             \ Set XX15+2 = normal_y
 STA XX15+2

 LDA XX12+4             \ Set A = normal_z

.LL93

 DEX                    \ Decrement the scale factor in X

 BMI LL94               \ If X was 0 before the decrement, there is no scaling
                        \ to do, so jump to LL94 to exit the loop

 LSR XX15               \ Set XX15 = XX15 / 2
                        \          = normal_x / 2

 LSR XX15+2             \ Set XX15+2 = XX15+2 / 2
                        \            = normal_y / 2

 LSR A                  \ Set A = A / 2
                        \       = normal_z / 2

 DEX                    \ Decrement the scale factor in X

 BPL LL93+3             \ If we have more scaling to do, loop back up to the
                        \ first LSR above until the normal vector is scaled down

.LL94

 STA R                  \ Set R = normal_z

 LDA XX12+5             \ Set S = normal_z_sign
 STA S

 LDA XX18+6             \ Set Q = dot_nosev_lo
 STA Q

 LDA XX18+8             \ Set A = dot_nosev_sign

 JSR LL38               \ Set (S A) = (S R) + (A Q)
                        \           = normal_z + dot_nosev
                        \
                        \ setting the sign of the result in S

 BCS ovflw              \ If the addition overflowed, jump up to ovflw to divide
                        \ both the normal vector and dot products by 2 and try
                        \ again

 STA XX15+4             \ Set XX15(5 4) = (S A)
 LDA S                  \               = normal_z + dot_nosev
 STA XX15+5

 LDA XX15               \ Set R = normal_x
 STA R

 LDA XX12+1             \ Set S = normal_x_sign
 STA S

 LDA XX18               \ Set Q = dot_sidev_lo
 STA Q

 LDA XX18+2             \ Set A = dot_sidev_sign

 JSR LL38               \ Set (S A) = (S R) + (A Q)
                        \           = normal_x + dot_sidev
                        \
                        \ setting the sign of the result in S

 BCS ovflw              \ If the addition overflowed, jump up to ovflw to divide
                        \ both the normal vector and dot products by 2 and try
                        \ again

 STA XX15               \ Set XX15(1 0) = (S A)
 LDA S                  \               = normal_x + dot_sidev
 STA XX15+1

 LDA XX15+2             \ Set R = normal_y
 STA R

 LDA XX12+3             \ Set S = normal_y_sign
 STA S

 LDA XX18+3             \ Set Q = dot_roofv_lo
 STA Q

 LDA XX18+5             \ Set A = dot_roofv_sign

 JSR LL38               \ Set (S A) = (S R) + (A Q)
                        \           = normal_y + dot_roofv

 BCS ovflw              \ If the addition overflowed, jump up to ovflw to divide
                        \ both the normal vector and dot products by 2 and try
                        \ again

 STA XX15+2             \ Set XX15(3 2) = (S A)
 LDA S                  \               = normal_y + dot_roofv
 STA XX15+3

.LL89

                        \ When we get here, we have set up the following:
                        \
                        \   XX15(1 0) = normal_x + dot_sidev
                        \             = normal_x + [x y z] . sidev
                        \
                        \   XX15(3 2) = normal_y + dot_roofv
                        \             = normal_y + [x y z] . roofv
                        \
                        \   XX15(5 4) = normal_z + dot_nosev
                        \             = normal_z + [x y z] . nosev
                        \
                        \ and:
                        \
                        \   XX12(1 0) = (normal_x_sign normal_x)
                        \
                        \   XX12(3 2) = (normal_y_sign normal_y)
                        \
                        \   XX12(5 4) = (normal_z_sign normal_z)
                        \
                        \ We now calculate the dot product XX12 . XX15 to tell
                        \ us whether or not this face is visible

 LDA XX12               \ Set Q = XX12
 STA Q

 LDA XX15               \ Set A = XX15

 JSR FMLTU              \ Set T = A * Q / 256
 STA T                  \       = XX15 * XX12 / 256

 LDA XX12+1             \ Set S = sign of XX15(1 0) * XX12(1 0), so:
 EOR XX15+1             \
 STA S                  \   (S T) = XX15(1 0) * XX12(1 0) / 256

 LDA XX12+2             \ Set Q = XX12+2
 STA Q

 LDA XX15+2             \ Set A = XX15+2

 JSR FMLTU              \ Set Q = A * Q
 STA Q                  \       = XX15+2 * XX12+2 / 256

 LDA T                  \ Set T = R, so now:
 STA R                  \
                        \   (S R) = XX15(1 0) * XX12(1 0) / 256

 LDA XX12+3             \ Set A = sign of XX15+3 * XX12+3, so:
 EOR XX15+3             \
                        \   (A Q) = XX15(3 2) * XX12(3 2) / 256

 JSR LL38               \ Set (S T) = (S R) + (A Q)
 STA T                  \           =   XX15(1 0) * XX12(1 0) / 256
                        \             + XX15(3 2) * XX12(3 2) / 256

 LDA XX12+4             \ Set Q = XX12+4
 STA Q

 LDA XX15+4             \ Set A = XX15+4

 JSR FMLTU              \ Set Q = A * Q
 STA Q                  \       = XX15+4 * XX12+4 / 256

 LDA T                  \ Set T = R, so now:
 STA R                  \
                        \   (S R) =   XX15(1 0) * XX12(1 0) / 256
                        \           + XX15(3 2) * XX12(3 2) / 256

 LDA XX15+5             \ Set A = sign of XX15+5 * XX12+5, so:
 EOR XX12+5             \
                        \   (A Q) = XX15(5 4) * XX12(5 4) / 256

 JSR LL38               \ Set (S A) = (S R) + (A Q)
                        \           =   XX15(1 0) * XX12(1 0) / 256
                        \             + XX15(3 2) * XX12(3 2) / 256
                        \             + XX15(5 4) * XX12(5 4) / 256

 PHA                    \ Push the result A onto the stack, so the stack now
                        \ contains the dot product XX12 . XX15

 TYA                    \ Set X = Y / 4
 LSR A                  \       = the number of this face * 4 /4
 LSR A                  \       = the number of this face
 TAX

 PLA                    \ Pull the dot product off the stack into A

 BIT S                  \ If bit 7 of S is set, i.e. the dot product is
 BMI P%+4               \ negative, then this face is visible as its normal is
                        \ pointing towards us, so skip the following instruction

 LDA #0                 \ Otherwise the face is not visible, so set A = 0 so we
                        \ can store this to mean "not visible"

 STA XX2,X              \ Store the face's visibility in the X-th byte of XX2

 INY                    \ Above we incremented Y to point to byte #3, so this
                        \ increments Y to point to byte #4, i.e. byte #0 of the
                        \ next face

.LL88

 CPY XX20               \ If Y >= XX20, the number of faces * 4, jump down to
 BCS LL42               \ LL42 to move on to the

 JMP LL86               \ Otherwise loop back to LL86 to work out the visibility
                        \ of the next face

\ ******************************************************************************
\
\       Name: LL9 (Part 6 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Calculate the visibility of each of the ship's vertices
\  Deep dive: Drawing ships
\             Calculating vertex coordinates
\
\ ------------------------------------------------------------------------------
\
\ This section calculates the visibility of each of the ship's vertices, and for
\ those that are visible, it starts the process of calculating the screen
\ coordinates of each vertex
\
\ ******************************************************************************

.LL42

                        \ The first task is to set up the inverse matrix, ready
                        \ for us to send to the dot product routine at LL51.
                        \ Back up in part 3, we set up the following variables:
                        \
                        \   * XX16(1 0) = sidev_x
                        \   * XX16(3 2) = sidev_y
                        \   * XX16(5 4) = sidev_z
                        \
                        \   * XX16(7 6) = roofv_x
                        \   * XX16(9 8) = roofv_y
                        \   * XX16(11 10) = roofv_z
                        \
                        \   * XX16(13 12) = nosev_x
                        \   * XX16(15 14) = nosev_y
                        \   * XX16(17 16) = nosev_z
                        \
                        \ and we then scaled the vectors to give the following:
                        \
                        \   * XX16   = scaled |sidev_x|
                        \   * XX16+2 = scaled |sidev_y|
                        \   * XX16+4 = scaled |sidev_z|
                        \
                        \   * XX16+6  = scaled |roofv_x|
                        \   * XX16+8  = scaled |roofv_y|
                        \   * XX16+10 = scaled |roofv_z|
                        \
                        \   * XX16+12 = scaled |nosev_x|
                        \   * XX16+14 = scaled |nosev_y|
                        \   * XX16+16 = scaled |nosev_z|
                        \
                        \ We now need to rearrange these locations so they
                        \ effectively transpose the matrix into its inverse

 LDY XX16+2             \ Set XX16+2 = XX16+6 = scaled |roofv_x|
 LDX XX16+3             \ Set XX16+3 = XX16+7 = roofv_x_hi
 LDA XX16+6             \ Set XX16+6 = XX16+2 = scaled |sidev_y|
 STA XX16+2             \ Set XX16+7 = XX16+3 = sidev_y_hi
 LDA XX16+7
 STA XX16+3
 STY XX16+6
 STX XX16+7

 LDY XX16+4             \ Set XX16+4 = XX16+12 = scaled |nosev_x|
 LDX XX16+5             \ Set XX16+5 = XX16+13 = nosev_x_hi
 LDA XX16+12            \ Set XX16+12 = XX16+4 = scaled |sidev_z|
 STA XX16+4             \ Set XX16+13 = XX16+5 = sidev_z_hi
 LDA XX16+13
 STA XX16+5
 STY XX16+12
 STX XX16+13

 LDY XX16+10            \ Set XX16+10 = XX16+14 = scaled |nosev_y|
 LDX XX16+11            \ Set XX16+11 = XX16+15 = nosev_y_hi
 LDA XX16+14            \ Set XX16+14 = XX16+10 = scaled |roofv_z|
 STA XX16+10            \ Set XX16+15 = XX16+11 = roofv_z
 LDA XX16+15
 STA XX16+11
 STY XX16+14
 STX XX16+15

                        \ So now we have the following sign-magnitude variables
                        \ containing parts of the scaled orientation vectors:
                        \
                        \   XX16(1 0)   = scaled sidev_x
                        \   XX16(3 2)   = scaled roofv_x
                        \   XX16(5 4)   = scaled nosev_x
                        \
                        \   XX16(7 6)   = scaled sidev_y
                        \   XX16(9 8)   = scaled roofv_y
                        \   XX16(11 10) = scaled nosev_y
                        \
                        \   XX16(13 12) = scaled sidev_z
                        \   XX16(15 14) = scaled roofv_z
                        \   XX16(17 16) = scaled nosev_z
                        \
                        \ which is what we want, as the various vectors are now
                        \ arranged so we can use LL51 to multiply by the
                        \ transpose (i.e. the inverse of the matrix)

 LDY #8                 \ Fetch byte #8 of the ship's blueprint, which is the
 LDA (XX0),Y            \ number of vertices * 8, and store it in XX20
 STA XX20

                        \ We now set V(1 0) = XX0(1 0) + 20, so V(1 0) points
                        \ to byte #20 of the ship's blueprint, which is always
                        \ where the vertex data starts (i.e. just after the 20
                        \ byte block that define the ship's characteristics)

 LDA XX0                \ We start with the low bytes
 CLC
 ADC #20
 STA V

 LDA XX0+1              \ And then do the high bytes
 ADC #0
 STA V+1

 LDY #0                 \ We are about to step through all the vertices, using
                        \ Y as a counter. There are six data bytes for each
                        \ vertex, so we will increment Y by 6 for each iteration
                        \ so it can act as an offset from V(1 0) to the current
                        \ vertex's data

 STY CNT                \ Set CNT = 0, which we will use as a pointer to the
                        \ heap at XX3, starting it at zero so the heap starts
                        \ out empty

.LL48

 STY XX17               \ Set XX17 = Y, so XX17 now contains the offset of the
                        \ current vertex's data

 LDA (V),Y              \ Fetch byte #0 for this vertex into XX15, so:
 STA XX15               \
                        \   XX15 = magnitude of the vertex's x-coordinate

 INY                    \ Increment Y to point to byte #1

 LDA (V),Y              \ Fetch byte #1 for this vertex into XX15+2, so:
 STA XX15+2             \
                        \   XX15+2 = magnitude of the vertex's y-coordinate

 INY                    \ Increment Y to point to byte #2

 LDA (V),Y              \ Fetch byte #2 for this vertex into XX15+4, so:
 STA XX15+4             \
                        \   XX15+4 = magnitude of the vertex's z-coordinate

 INY                    \ Increment Y to point to byte #3

 LDA (V),Y              \ Fetch byte #3 for this vertex into T, so:
 STA T                  \
                        \   T = %xyz vvvvv, where:
                        \
                        \     * Bits 0-4 = visibility distance, beyond which the
                        \                  vertex is not shown
                        \
                        \     * Bits 7-5 = the sign bits of x, y and z

 AND #%00011111         \ Extract bits 0-4 to get the visibility distance

 CMP XX4                \ If XX4 > the visibility distance, where XX4 contains
 BCC LL49-3             \ the ship's z-distance reduced to 0-31 (which we set in
                        \ part 2), then this vertex is too far away to be
                        \ visible, so jump down to LL50 (via the JMP instruction
                        \ in LL49-3) to move on to the next vertex

 INY                    \ Increment Y to point to byte #4

 LDA (V),Y              \ Fetch byte #4 for this vertex into P, so:
 STA P                  \
                        \  P = %ffff ffff, where:
                        \
                        \    * Bits 0-3 = the number of face 1
                        \
                        \    * Bits 4-7 = the number of face 2

 AND #%00001111         \ Extract the number of face 1 into X
 TAX

 LDA XX2,X              \ If XX2+X is non-zero then we decided in part 5 that
 BNE LL49               \ face 1 is visible, so jump to LL49

 LDA P                  \ Fetch byte #4 for this vertex into A

 LSR A                  \ Shift right four times to extract the number of face 2
 LSR A                  \ from bits 4-7 into X
 LSR A
 LSR A
 TAX

 LDA XX2,X              \ If XX2+X is non-zero then we decided in part 5 that
 BNE LL49               \ face 2 is visible, so jump to LL49

 INY                    \ Increment Y to point to byte #5

 LDA (V),Y              \ Fetch byte #5 for this vertex into P, so:
 STA P                  \
                        \  P = %ffff ffff, where:
                        \
                        \    * Bits 0-3 = the number of face 3
                        \
                        \    * Bits 4-7 = the number of face 4

 AND #%00001111         \ Extract the number of face 1 into X
 TAX

 LDA XX2,X              \ If XX2+X is non-zero then we decided in part 5 that
 BNE LL49               \ face 3 is visible, so jump to LL49

 LDA P                  \ Fetch byte #5 for this vertex into A

 LSR A                  \ Shift right four times to extract the number of face 4
 LSR A                  \ from bits 4-7 into X
 LSR A
 LSR A
 TAX

 LDA XX2,X              \ If XX2+X is non-zero then we decided in part 5 that
 BNE LL49               \ face 4 is visible, so jump to LL49

 JMP LL50               \ If we get here then none of the four faces associated
                        \ with this vertex are visible, so this vertex is also
                        \ not visible, so jump to LL50 to move on to the next
                        \ vertex

.LL49

 LDA T                  \ Fetch byte #5 for this vertex into A and store it, so
 STA XX15+1             \ XX15+1 now has the sign of the vertex's x-coordinate

 ASL A                  \ Shift A left and store it, so XX15+3 now has the sign
 STA XX15+3             \ of the vertex's y-coordinate

 ASL A                  \ Shift A left and store it, so XX15+5 now has the sign
 STA XX15+5             \ of the vertex's z-coordinate

                        \ By this point we have the following:
                        \
                        \   XX15(1 0) = vertex x-coordinate
                        \   XX15(3 2) = vertex y-coordinate
                        \   XX15(5 4) = vertex z-coordinate
                        \
                        \   XX16(1 0)   = scaled sidev_x
                        \   XX16(3 2)   = scaled roofv_x
                        \   XX16(5 4)   = scaled nosev_x
                        \
                        \   XX16(7 6)   = scaled sidev_y
                        \   XX16(9 8)   = scaled roofv_y
                        \   XX16(11 10) = scaled nosev_y
                        \
                        \   XX16(13 12) = scaled sidev_z
                        \   XX16(15 14) = scaled roofv_z
                        \   XX16(17 16) = scaled nosev_z

 JSR LL51               \ Call LL51 to set XX12 to the dot products of XX15 and
                        \ XX16, as follows:
                        \
                        \   XX12(1 0) = [ x y z ] . [ sidev_x roofv_x nosev_x ]
                        \
                        \   XX12(3 2) = [ x y z ] . [ sidev_y roofv_y nosev_y ]
                        \
                        \   XX12(5 4) = [ x y z ] . [ sidev_z roofv_z nosev_z ]
                        \
                        \ XX12 contains the vector from the ship's centre to
                        \ the vertex, transformed from the orientation vector
                        \ space to the universe orientated around our ship. So
                        \ we can refer to this vector below, let's call it
                        \ vertv, so:
                        \
                        \   vertv_x = [ x y z ] . [ sidev_x roofv_x nosev_x ]
                        \
                        \   vertv_y = [ x y z ] . [ sidev_y roofv_y nosev_y ]
                        \
                        \   vertv_z = [ x y z ] . [ sidev_z roofv_z nosev_z ]
                        \
                        \ To finish the calculation, we now want to calculate:
                        \
                        \   vertv + [ x y z ]
                        \
                        \ So let's start with the vertv_x + x

 LDA XX1+2              \ Set A = x_sign of the ship's location

 STA XX15+2             \ Set XX15+2 = x_sign

 EOR XX12+1             \ If the sign of x_sign * the sign of vertv_x is
 BMI LL52               \ negative (i.e. they have different signs), skip to
                        \ LL52

 CLC                    \ Set XX15(2 1 0) = XX1(2 1 0) + XX12(1 0)
 LDA XX12               \                 = (x_sign x_hi x_lo) + vertv_x
 ADC XX1                \
 STA XX15               \ Starting with the low bytes

 LDA XX1+1              \ And then doing the high bytes (we can add 0 here as
 ADC #0                 \ we know the sign byte of vertv_x is 0)
 STA XX15+1

 JMP LL53               \ We've added the x-coordinates, so jump to LL53 to do
                        \ the y-coordinates

.LL52

                        \ If we get here then x_sign and vertv_x have different
                        \ signs, so we need to subtract them to get the result

 LDA XX1                \ Set XX15(2 1 0) = XX1(2 1 0) - XX12(1 0)
 SEC                    \                 = (x_sign x_hi x_lo) - vertv_x
 SBC XX12               \
 STA XX15               \ Starting with the low bytes

 LDA XX1+1              \ And then doing the high bytes (we can subtract 0 here
 SBC #0                 \ as we know the sign byte of vertv_x is 0)
 STA XX15+1

 BCS LL53               \ If the subtraction didn't underflow, then the sign of
                        \ the result is the same sign as x_sign, and that's what
                        \ we want, so we can jump down to LL53 to do the
                        \ y-coordinates

 EOR #%11111111         \ Otherwise we need to negate the result using two's
 STA XX15+1             \ complement, so first we flip the bits of the high byte

 LDA #1                 \ And then subtract the low byte from 1
 SBC XX15
 STA XX15

 BCC P%+5               \ If the above subtraction underflowed then we need to
 INC XX15+1             \ bump the high byte of the result up by 1

 LDA XX15+2             \ And now we flip the sign of the result to get the
 EOR #%10000000         \ correct result
 STA XX15+2

.LL53

                        \ Now for the y-coordinates, vertv_y + y

 LDA XX1+5              \ Set A = y_sign of the ship's location

 STA XX15+5             \ Set XX15+5 = y_sign

 EOR XX12+3             \ If the sign of y_sign * the sign of vertv_y is
 BMI LL54               \ negative (i.e. they have different signs), skip to
                        \ LL54

 CLC                    \ Set XX15(5 4 3) = XX1(5 4 3) + XX12(3 2)
 LDA XX12+2             \                 = (y_sign y_hi y_lo) + vertv_y
 ADC XX1+3              \
 STA XX15+3             \ Starting with the low bytes

 LDA XX1+4              \ And then doing the high bytes (we can add 0 here as
 ADC #0                 \ we know the sign byte of vertv_y is 0)
 STA XX15+4

 JMP LL55               \ We've added the y-coordinates, so jump to LL55 to do
                        \ the z-coordinates

.LL54

                        \ If we get here then y_sign and vertv_y have different
                        \ signs, so we need to subtract them to get the result

 LDA XX1+3              \ Set XX15(5 4 3) = XX1(5 4 3) - XX12(3 2)
 SEC                    \                 = (y_sign y_hi y_lo) - vertv_y
 SBC XX12+2             \
 STA XX15+3             \ Starting with the low bytes

 LDA XX1+4              \ And then doing the high bytes (we can subtract 0 here
 SBC #0                 \ as we know the sign byte of vertv_z is 0)
 STA XX15+4

 BCS LL55               \ If the subtraction didn't underflow, then the sign of
                        \ the result is the same sign as y_sign, and that's what
                        \ we want, so we can jump down to LL55 to do the
                        \ z-coordinates

 EOR #%11111111         \ Otherwise we need to negate the result using two's
 STA XX15+4             \ complement, so first we flip the bits of the high byte

 LDA XX15+3             \ And then flip the bits of the low byte and add 1
 EOR #%11111111
 ADC #1
 STA XX15+3

 LDA XX15+5             \ And now we flip the sign of the result to get the
 EOR #%10000000         \ correct result
 STA XX15+5

 BCC LL55               \ If the above subtraction underflowed then we need to
 INC XX15+4             \ bump the high byte of the result up by 1

.LL55

                        \ Now for the z-coordinates, vertv_z + z

 LDA XX12+5             \ If vertv_z_hi is negative, jump down to LL56
 BMI LL56

 LDA XX12+4             \ Set (U T) = XX1(7 6) + XX12(5 4)
 CLC                    \           = (z_hi z_lo) + vertv_z
 ADC XX1+6              \
 STA T                  \ Starting with the low bytes

 LDA XX1+7              \ And then doing the high bytes (we can add 0 here as
 ADC #0                 \ we know the sign byte of vertv_y is 0)
 STA U

 JMP LL57               \ We've added the z-coordinates, so jump to LL57

                        \ The adding process is continued in part 7, after a
                        \ couple of subroutines that we don't need quite yet

\ ******************************************************************************
\
\       Name: LL61
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (U R) = 256 * A / Q
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following, where A >= Q:
\
\   (U R) = 256 * A / Q
\
\ This is a sister routine to LL28, which does the division when A < Q.
\
\ ******************************************************************************

.LL61

 LDX Q                  \ If Q = 0, jump down to LL84 to return a division
 BEQ LL84               \ error

                        \ The LL28 routine returns A / Q, but only if A < Q. In
                        \ our case A >= Q, but we still want to use the LL28
                        \ routine, so we halve A until it's less than Q, call
                        \ the division routine, and then double A by the same
                        \ number of times

 LDX #0                 \ Set X = 0 to count the number of times we halve A

.LL63

 LSR A                  \ Halve A by shifting right

 INX                    \ Increment X

 CMP Q                  \ If A >= Q, loop back to LL63 to halve it again
 BCS LL63

 STX S                  \ Otherwise store the number of times we halved A in S

 JSR LL28               \ Call LL28 to calculate:
                        \
                        \   R = 256 * A / Q
                        \
                        \ which we can do now as A < Q

 LDX S                  \ Otherwise restore the number of times we halved A
                        \ above into X

 LDA R                  \ Set A = our division result

.LL64

 ASL A                  \ Double (U A) by shifting left
 ROL U

 BMI LL84               \ If bit 7 of U is set, the doubling has overflowed, so
                        \ jump to LL84 to return a division error

 DEX                    \ Decrement X

 BNE LL64               \ If X is not yet zero then we haven't done as many
                        \ doublings as we did halvings earlier, so loop back for
                        \ another doubling

 STA R                  \ Store the low byte of the division result in R

 RTS                    \ Return from the subroutine

.LL84

 LDA #50                \ If we get here then either we tried to divide by 0, or
 STA R                  \ the result overflowed, so we set U and R to 50
 STA U

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL62
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate 128 - (U R)
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following for a positive sign-magnitude number (U R):
\
\   128 - (U R)
\
\ and then store the result, low byte then high byte, on the end of the heap at
\ XX3, where X points to the first free byte on the heap. Return by jumping down
\ to LL66.
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   X                   X is incremented by 1
\
\ ******************************************************************************

.LL62

 LDA #128               \ Calculate 128 - (U R), starting with the low bytes
 SEC
 SBC R

 STA XX3,X              \ Store the low byte of the result in the X-th byte of
                        \ the heap at XX3

 INX                    \ Increment the heap pointer in X to point to the next
                        \ byte

 LDA #0                 \ And then subtract the high bytes
 SBC U

 STA XX3,X              \ Store the low byte of the result in the X-th byte of
                        \ the heap at XX3

 JMP LL66               \ Jump down to LL66

\ ******************************************************************************
\
\       Name: LL9 (Part 7 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Calculate the visibility of each of the ship's vertices
\  Deep dive: Drawing ships
\             Calculating vertex coordinates
\
\ ------------------------------------------------------------------------------
\
\ This section continues the coordinate adding from part 6 by finishing off the
\ calculation that we started above:
\
\                      [ sidev_x roofv_x nosev_x ]   [ x ]   [ x ]
\   vector to vertex = [ sidev_y roofv_y nosev_y ] . [ y ] + [ y ]
\                      [ sidev_z roofv_z nosev_z ]   [ z ]   [ z ]
\
\ The gets stored as follows, in sign-magnitude values with the magnitudes
\ fitting into the low bytes:
\
\   XX15(2 0)           [ x y z ] . [ sidev_x roofv_x nosev_x ] + [ x y z ]
\
\   XX15(5 3)           [ x y z ] . [ sidev_y roofv_y nosev_y ] + [ x y z ]
\
\   (U T)               [ x y z ] . [ sidev_z roofv_z nosev_z ] + [ x y z ]
\
\ Finally, because this vector is from our ship to the vertex, and we are at the
\ origin, this vector is the same as the coordinates of the vertex. In other
\ words, we have just worked out:
\
\   XX15(2 0)           x-coordinate of the current vertex
\
\   XX15(5 3)           y-coordinate of the current vertex
\
\   (U T)               z-coordinate of the current vertex
\
\ ******************************************************************************

.LL56

 LDA XX1+6              \ Set (U T) = XX1(7 6) - XX12(5 4)
 SEC                    \           = (z_hi z_lo) - vertv_z
 SBC XX12+4             \
 STA T                  \ Starting with the low bytes

 LDA XX1+7              \ And then doing the high bytes (we can subtract 0 here
 SBC #0                 \ as we know the sign byte of vertv_z is 0)
 STA U

 BCC LL140              \ If the subtraction just underflowed, skip to LL140 to
                        \ set (U T) to the minimum value of 4

 BNE LL57               \ If U is non-zero, jump down to LL57

 LDA T                  \ If T >= 4, jump down to LL57
 CMP #4
 BCS LL57

.LL140

 LDA #0                 \ If we get here then either (U T) < 4 or the
 STA U                  \ subtraction underflowed, so set (U T) = 4
 LDA #4
 STA T

.LL57

                        \ By this point we have our results, so now to scale
                        \ the 16-bit results down into 8-bit values

 LDA U                  \ If the high bytes of the result are all zero, we are
 ORA XX15+1             \ done, so jump down to LL60 for the next stage
 ORA XX15+4
 BEQ LL60

 LSR XX15+1             \ Shift XX15(1 0) to the right
 ROR XX15

 LSR XX15+4             \ Shift XX15(4 3) to the right
 ROR XX15+3

 LSR U                  \ Shift (U T) to the right
 ROR T

 JMP LL57               \ Jump back to LL57 to see if we can shift the result
                        \ any more

\ ******************************************************************************
\
\       Name: LL9 (Part 8 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Calculate the screen coordinates of visible vertices
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This section projects the coordinate of the vertex into screen coordinates and
\ stores them on the XX3 heap. By the end of this part, the XX3 heap contains
\ four bytes containing the 16-bit screen coordinates of the current vertex, in
\ the order: x_lo, x_hi, y_lo, y_hi.
\
\ When we reach here, we are looping through the vertices, and we've just worked
\ out the coordinates of the vertex in our normal coordinate system, as follows
\
\   XX15(2 0)           (x_sign x_lo) = x-coordinate of the current vertex
\
\   XX15(5 3)           (y_sign y_lo) = y-coordinate of the current vertex
\
\   (U T)               (z_sign z_lo) = z-coordinate of the current vertex
\
\ Note that U is always zero when we get to this point, as the vertex is always
\ in front of us (so it has a positive z-coordinate, into the screen).
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   LL70+1              Contains an RTS (as the first byte of an LDA
\                       instruction)
\
\   LL66                A re-entry point into the ship-drawing routine, used by
\                       the LL62 routine to store 128 - (U R) on the XX3 heap
\
\ ******************************************************************************

.LL60

 LDA T                  \ Set Q = z_lo
 STA Q

 LDA XX15               \ Set A = x_lo

 CMP Q                  \ If x_lo < z_lo jump to LL69
 BCC LL69

 JSR LL61               \ Call LL61 to calculate:
                        \
                        \   (U R) = 256 * A / Q
                        \         = 256 * x / z
                        \
                        \ which we can do as x >= z

 JMP LL65               \ Jump to LL65 to skip the division for x_lo < z_lo

.LL69

 JSR LL28               \ Call LL28 to calculate:
                        \
                        \   R = 256 * A / Q
                        \     = 256 * x / z
                        \
                        \ Because x < z, the result fits into one byte, and we
                        \ also know that U = 0, so (U R) also contains the
                        \ result

.LL65

                        \ At this point we have:
                        \
                        \   (U R) = x / z
                        \
                        \ so (U R) contains the vertex's x-coordinate projected
                        \ on screen
                        \
                        \ The next task is to convert (U R) to a pixel screen
                        \ coordinate and stick it on the XX3 heap.
                        \
                        \ We start with the x-coordinate. To convert the
                        \ x-coordinate to a screen pixel we add 128, the
                        \ x-coordinate of the centre of the screen, because the
                        \ projected value is relative to an origin at the centre
                        \ of the screen, but the origin of the screen pixels is
                        \ at the top-left of the screen

 LDX CNT                \ Fetch the pointer to the end of the XX3 heap from CNT
                        \ into X

 LDA XX15+2             \ If x_sign is negative, jump up to LL62, which will
 BMI LL62               \ store 128 - (U R) on the XX3 heap and return by
                        \ jumping down to LL66 below

 LDA R                  \ Calculate 128 + (U R), starting with the low bytes
 CLC
 ADC #128

 STA XX3,X              \ Store the low byte of the result in the X-th byte of
                        \ the heap at XX3

 INX                    \ Increment the heap pointer in X to point to the next
                        \ byte

 LDA U                  \ And then add the high bytes
 ADC #0

 STA XX3,X              \ Store the high byte of the result in the X-th byte of
                        \ the heap at XX3

.LL66

                        \ We've just stored the screen x-coordinate of the
                        \ vertex on the XX3 heap, so now for the y-coordinate

 TXA                    \ Store the heap pointer in X on the stack (at this
 PHA                    \ it points to the last entry on the heap, not the first
                        \ free byte)

 LDA #0                 \ Set U = 0
 STA U

 LDA T                  \ Set Q = z_lo
 STA Q

 LDA XX15+3             \ Set A = y_lo

 CMP Q                  \ If y_lo < z_lo jump to LL67
 BCC LL67

 JSR LL61               \ Call LL61 to calculate:
                        \
                        \   (U R) = 256 * A / Q
                        \         = 256 * y / z
                        \
                        \ which we can do as y >= z

 JMP LL68               \ Jump to LL68 to skip the division for y_lo < z_lo

.LL70

                        \ This gets called from below when y_sign is negative

 LDA #Y                 \ Calculate #Y + (U R), starting with the low bytes
 CLC
 ADC R

 STA XX3,X              \ Store the low byte of the result in the X-th byte of
                        \ the heap at XX3

 INX                    \ Increment the heap pointer in X to point to the next
                        \ byte

 LDA #0                 \ And then add the high bytes
 ADC U

 STA XX3,X              \ Store the high byte of the result in the X-th byte of
                        \ the heap at XX3

 JMP LL50               \ Jump to LL50 to move on to the next vertex

.LL67

 JSR LL28               \ Call LL28 to calculate:
                        \
                        \   R = 256 * A / Q
                        \     = 256 * y / z
                        \
                        \ Because y < z, the result fits into one byte, and we
                        \ also know that U = 0, so (U R) also contains the
                        \ result

.LL68

                        \ At this point we have:
                        \
                        \   (U R) = y / z
                        \
                        \ so (U R) contains the vertex's y-coordinate projected
                        \ on screen
                        \
                        \ We now want to convert this to a screen y-coordinate
                        \ and stick it on the XX3 heap, much like we did with
                        \ the x-coordinate above. Again, we convert the
                        \ coordinate by adding or subtracting the y-coordinate
                        \ of the centre of the screen, which is in the constant
                        \ #Y, but this time we do the opposite, as a positive
                        \ projected y-coordinate, i.e. up the space y-axis and
                        \ up the screen, converts to a low y-coordinate, which
                        \ is the opposite way round to the x-coordinates

 PLA                    \ Restore the heap pointer from the stack into X
 TAX

 INX                    \ When we stored the heap pointer, it pointed to the
                        \ last entry on the heap, not the first free byte, so we
                        \ increment it so it does point to the next free byte

 LDA XX15+5             \ If y_sign is negative, jump up to LL70, which will
 BMI LL70               \ store #Y + (U R) on the XX3 heap and return by jumping
                        \ down to LL50 below

 LDA #Y                 \ Calculate #Y - (U R), starting with the low bytes
 SEC
 SBC R

 STA XX3,X              \ Store the low byte of the result in the X-th byte of
                        \ the heap at XX3

 INX                    \ Increment the heap pointer in X to point to the next
                        \ byte

 LDA #0                 \ And then subtract the high bytes
 SBC U

 STA XX3,X              \ Store the high byte of the result in the X-th byte of
                        \ the heap at XX3

.LL50

                        \ By the time we get here, the XX3 heap contains four
                        \ bytes containing the screen coordinates of the current
                        \ vertex, in the order: x_lo, x_hi, y_lo, y_hi

 CLC                    \ Set CNT = CNT + 4, so the heap pointer points to the
 LDA CNT                \ next free byte on the heap
 ADC #4
 STA CNT

 LDA XX17               \ Set A to the offset of the current vertex's data,
                        \ which we set in part 6

 ADC #6                 \ Set Y = A + 6, so Y now points to the data for the
 TAY                    \ next vertex

 BCS LL72               \ If the addition just overflowed, meaning we just tried
                        \ to access vertex #43, jump to LL72, as the maximum
                        \ number of vertices allowed is 42

 CMP XX20               \ If Y >= number of vertices * 6 (which we stored in
 BCS LL72               \ XX20 in part 6), jump to LL72, as we have processed
                        \ all the vertices for this ship

 JMP LL48               \ Loop back to LL48 in part 6 to calculate visibility
                        \ and screen coordinates for the next vertex

\ ******************************************************************************
\
\       Name: LL9 (Part 9 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Draw laser beams if the ship is firing its laser at us
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This part sets things up so we can loop through the edges in the next part. It
\ also adds a line to the ship line heap, if the ship is firing at us.
\
\ When we get here, the heap at XX3 contains all the visible vertex screen
\ coordinates.
\
\ ******************************************************************************

.LL72

.EE31

 LDY #9                 \ Fetch byte #9 of the ship's blueprint, which is the
 LDA (XX0),Y            \ number of edges, and store it in XX20
 STA XX20

 LDA #%00001000         \ Set bit 3 of A so the next instruction sets bit 3 of
                        \ the ship's byte #31 to denote that we are drawing
                        \ something on-screen for this ship

.LL74

 ORA XX1+31             \ Apply bit 3 of A to the ship's byte #31, so if there
 STA XX1+31             \ was no ship already on screen, the bit is clear,
                        \ otherwise it is set

 LDY #0                 \ Set XX17 = 0, which we are going to use as a counter
 STY XX17               \ for stepping through the ship's edges

 BIT XX1+31             \ If bit 6 of the ship's byte #31 is clear, then the
 BVC LL170              \ ship is not firing its lasers, so jump to LL170 to
                        \ skip the drawing of laser lines

                        \ The ship is firing its laser at us, so we need to draw
                        \ the laser lines

 LDA XX1+31             \ Clear bit 6 of the ship's byte #31 so the ship doesn't
 AND #%10111111         \ keep firing endlessly
 STA XX1+31

 LDY #6                 \ Fetch byte #6 of the ship's blueprint, which is the
 LDA (XX0),Y            \ number * 4 of the vertex where the ship has its lasers

 TAY                    \ Put the vertex number into Y, where it can act as an
                        \ index into list of vertex screen coordinates we added
                        \ to the XX3 heap

 LDX XX3,Y              \ Fetch the x_lo coordinate of the laser vertex from the
 STX XX15               \ XX3 heap into XX15

 INX                    \ If X = 255 then the laser vertex is not visible, as
 BEQ LL170              \ the value we stored in part 2 wasn't overwritten by
                        \ the vertex calculation in part 6 and 7, so jump to
                        \ LL170 to skip drawing the laser lines

                        \ We now build a laser beam from the ship's laser vertex
                        \ towards our ship, as follows:
                        \
                        \   XX15(1 0) = laser vertex x-coordinate
                        \
                        \   XX15(3 2) = laser vertex y-coordinate
                        \
                        \   XX15(5 4) = x-coordinate of the end of the beam
                        \
                        \   XX12(1 0) = y-coordinate of the end of the beam
                        \
                        \ The end of the laser beam will be positioned to look
                        \ good, rather than being directly aimed at us, as
                        \ otherwise we would only see a flashing point of light
                        \ as they unleashed their attack

 LDX XX3+1,Y            \ Fetch the x_hi coordinate of the laser vertex from the
 STX XX15+1             \ XX3 heap into XX15+1

 INX                    \ If X = 255 then the laser vertex is not visible, as
 BEQ LL170              \ the value we stored in part 2 wasn't overwritten by
                        \ a vertex calculation in part 6 and 7, so jump to LL170
                        \ to skip drawing the laser beam

 LDX XX3+2,Y            \ Fetch the y_lo coordinate of the laser vertex from the
 STX XX15+2             \ XX3 heap into XX15+2

 LDX XX3+3,Y            \ Fetch the y_hi coordinate of the laser vertex from the
 STX XX15+3             \ XX3 heap into XX15+3

 LDA #0                 \ Set XX15(5 4) = 0, so their laser beam fires to the
 STA XX15+4             \ left edge of the screen
 STA XX15+5

 STA XX12+1             \ Set XX12(1 0) = the ship's z_lo coordinate, which will
 LDA XX1+6              \ effectively make the vertical position of the end of
 STA XX12               \ the laser beam move around as the ship moves in space

 LDA XX1+2              \ If the ship's x_sign is positive, skip the next
 BPL P%+5               \ instruction

 DEC XX15+4             \ The ship's x_sign is negative (i.e. it's on the left
                        \ side of the screen), so switch the laser beam so it
                        \ goes to the right edge of the screen by decrementing
                        \ XX15(5 4) to 255

 JSR LL145              \ Call LL145 to see if the laser beam needs to be
                        \ clipped to fit on-screen, returning the clipped line's
                        \ end-points in (X1, Y1) and (X2, Y2)

 BCS LL170              \ If the C flag is set then the line is not visible on
                        \ screen, so jump to LL170 so we don't store this line
                        \ in the ship line heap

 JSR LSPUT              \ Draw the laser line using smooth animation, by first
                        \ drawing the new laser line and then erasing the
                        \ corresponding old line from the screen

\ ******************************************************************************
\
\       Name: LL9 (Part 10 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Calculate the visibility of each of the ship's edges
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This part calculates which edges are visible - in other words, which lines we
\ should draw - and clips them to fit on the screen.
\
\ When we get here, the heap at XX3 contains all the visible vertex screen
\ coordinates.
\
\ ******************************************************************************

.LL170

 LDY #3                 \ Fetch byte #3 of the ship's blueprint, which contains
 CLC                    \ the low byte of the offset to the edges data
 LDA (XX0),Y

 ADC XX0                \ Set V = low byte edges offset + XX0
 STA V

 LDY #16                \ Fetch byte #16 of the ship's blueprint, which contains
 LDA (XX0),Y            \ the high byte of the offset to the edges data

 ADC XX0+1              \ Set V+1 = high byte edges offset + XX0+1
 STA V+1                \
                        \ So V(1 0) now points to the start of the edges data
                        \ for this ship

 LDY #5                 \ Fetch byte #5 of the ship's blueprint, which contains
 LDA (XX0),Y            \ the maximum heap size for plotting the ship (which is
 STA CNT                \ 1 + 4 * the maximum number of visible edges) and store
                        \ it in CNT

.LL75

 LDY #0                 \ Set Y = 0 so we start with byte #0

 LDA (V),Y              \ Fetch byte #0 for this edge, which contains the
                        \ visibility distance for this edge, beyond which the
                        \ edge is not shown

 CMP XX4                \ If XX4 > the visibility distance, where XX4 contains
 BCC LL78               \ the ship's z-distance reduced to 0-31 (which we set in
                        \ part 2), then this edge is too far away to be visible,
                        \ so jump down to LL78 to move on to the next edge

 INY                    \ Increment Y to point to byte #1

 LDA (V),Y              \ Fetch byte #1 for this edge into A, so:
                        \
                        \   A = %ffff ffff, where:
                        \
                        \     * Bits 0-3 = the number of face 1
                        \
                        \     * Bits 4-7 = the number of face 2

 STA P                  \ Store byte #1 into P

 AND #%00001111         \ Extract the number of face 1 into X
 TAX

 LDA XX2,X              \ If XX2+X is non-zero then we decided in part 5 that
 BNE LL79               \ face 1 is visible, so jump to LL79

 LDA P                  \ Fetch byte #1 for this edge into A

 LSR A                  \ Shift right four times to extract the number of face 2
 LSR A                  \ from bits 4-7 into X
 LSR A
 LSR A
 TAX

 LDA XX2,X              \ If XX2+X is zero then we decided in part 5 that
 BEQ LL78               \ face 2 is hidden, so jump to LL78

.LL79

                        \ We now build the screen line for this edge, as
                        \ follows:
                        \
                        \   XX15(1 0) = start x-coordinate
                        \
                        \   XX15(3 2) = start y-coordinate
                        \
                        \   XX15(5 4) = end x-coordinate
                        \
                        \   XX12(1 0) = end y-coordinate
                        \
                        \ We can then pass this to the line clipping routine
                        \ before storing the resulting line in the ship line
                        \ heap

 INY                    \ Increment Y to point to byte #2

 LDA (V),Y              \ Fetch byte #2 for this edge into X, which contains
 TAX                    \ the number of the vertex at the start of the edge

 LDA XX3+1,X            \ Fetch the x_hi coordinate of the edge's start vertex
 STA XX15+1             \ from the XX3 heap into XX15+1

 LDA XX3,X              \ Fetch the x_lo coordinate of the edge's start vertex
 STA XX15               \ from the XX3 heap into XX15

 LDA XX3+2,X            \ Fetch the y_lo coordinate of the edge's start vertex
 STA XX15+2             \ from the XX3 heap into XX15+2

 LDA XX3+3,X            \ Fetch the y_hi coordinate of the edge's start vertex
 STA XX15+3             \ from the XX3 heap into XX15+3

 INY                    \ Increment Y to point to byte #3

 LDA (V),Y              \ Fetch byte #3 for this edge into X, which contains
 TAX                    \ the number of the vertex at the end of the edge

 LDA XX3,X              \ Fetch the x_lo coordinate of the edge's end vertex
 STA XX15+4             \ from the XX3 heap into XX15+4

 LDA XX3+3,X            \ Fetch the y_hi coordinate of the edge's end vertex
 STA XX12+1             \ from the XX3 heap into XX12+1

 LDA XX3+2,X            \ Fetch the y_lo coordinate of the edge's end vertex
 STA XX12               \ from the XX3 heap into XX12

 LDA XX3+1,X            \ Fetch the x_hi coordinate of the edge's end vertex
 STA XX15+5             \ from the XX3 heap into XX15+5

 JSR LL147              \ Call LL147 to see if the new line segment needs to be
                        \ clipped to fit on-screen, returning the clipped line's
                        \ end-points in (X1, Y1) and (X2, Y2)

 BCS LL78               \ If the C flag is set then the line is not visible on
                        \ screen, so jump to LL78 so we don't store this line
                        \ in the ship line heap

 JSR LSPUT              \ Draw this edge using smooth animation, by first
                        \ drawing the ship's new line and then erasing the
                        \ corresponding old line from the screen

\ ******************************************************************************
\
\       Name: LL9 (Part 11 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Add all visible edges to the ship line heap
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This part adds all the visible edges to the ship line heap, so we can draw
\ them in part 12.
\
\ Other entry points:
\
\   LL81+2              Draw the contents of the ship line heap, used to draw
\                       the ship as a dot from SHPPT
\
\ ******************************************************************************

.LL78

 LDA LSNUM              \ If LSNUM >= CNT, skip to LL81 so we don't loop back
 CMP CNT                \ for the next edge (CNT was set to the maximum heap
 BCS LL81               \ size for this ship in part 10, so this checks whether
                        \ we have just run out of space in the ship line heap,
                        \ and stops drawing edges if we have)

 LDA V                  \ Increment V by 4 so V(1 0) points to the data for the
 CLC                    \ next edge
 ADC #4
 STA V

 BCC ll81               \ If the above addition didn't overflow, jump to ll81

 INC V+1                \ Otherwise increment the high byte of V(1 0), as we
                        \ just moved the V(1 0) pointer past a page boundary

.ll81

 INC XX17               \ Increment the edge counter to point to the next edge

 LDY XX17               \ If Y < XX20, which contains the number of edges in
 CPY XX20               \ the blueprint, loop back to LL75 to process the next
 BCC LL75               \ edge

.LL81

 JMP LL155              \ Jump down to part 12 below to draw any remaining lines
                        \ from the old ship that are still in the ship line heap

\ ******************************************************************************
\
\       Name: LL145 (Part 1 of 4)
\       Type: Subroutine
\   Category: Drawing lines
\    Summary: Clip line: Work out which end-points are on-screen, if any
\  Deep dive: Line-clipping
\             Extended screen coordinates
\
\ ------------------------------------------------------------------------------
\
\ This routine clips the line from (x1, y1) to (x2, y2) so it fits on-screen, or
\ returns an error if it can't be clipped to fit. The arguments are 16-bit
\ coordinates, and the clipped line is returned using 8-bit screen coordinates.
\
\ This part sets XX13 to reflect which of the two points are on-screen and
\ off-screen.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   XX15(1 0)           x1 as a 16-bit coordinate (x1_hi x1_lo)
\
\   XX15(3 2)           y1 as a 16-bit coordinate (y1_hi y1_lo)
\
\   XX15(5 4)           x2 as a 16-bit coordinate (x2_hi x2_lo)
\
\   XX12(1 0)           y2 as a 16-bit coordinate (y2_hi y2_lo)
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   (X1, Y1)            Screen coordinate of the start of the clipped line
\
\   (X2, Y2)            Screen coordinate of the end of the clipped line
\
\   C flag              Clear if the clipped line fits on-screen, set if it
\                       doesn't
\
\   XX13                The state of the original coordinates on-screen:
\
\                         * 0   = (x2, y2) on-screen
\
\                         * 95  = (x1, y1) on-screen,  (x2, y2) off-screen
\
\                         * 191 = (x1, y1) off-screen, (x2, y2) off-screen
\
\                       So XX13 is non-zero if the end of the line was clipped,
\                       meaning the next line sent to BLINE can't join onto the
\                       end but has to start a new segment
\
\   SWAP                The swap status of the returned coordinates:
\
\                         * &FF if we swapped the values of (x1, y1) and
\                           (x2, y2) as part of the clipping process
\
\                         * 0 if the coordinates are still in the same order
\
\   Y                   Y is preserved
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   LL147               Don't initialise the values in SWAP or A
\
\ ******************************************************************************

.LL145

 LDA #0                 \ Set SWAP = 0
 STA SWAP

 LDA XX15+5             \ Set A = x2_hi

.LL147

 LDX #Y*2-1             \ Set X = #Y * 2 - 1. The constant #Y is 96, the
                        \ y-coordinate of the mid-point of the space view, so
                        \ this sets Y2 to 191, the y-coordinate of the bottom
                        \ pixel row of the space view

 ORA XX12+1             \ If one or both of x2_hi and y2_hi are non-zero, jump
 BNE LL107              \ to LL107 to skip the following, leaving X at 191

 CPX XX12               \ If y2_lo > the y-coordinate of the bottom of screen
 BCC LL107              \ then (x2, y2) is off the bottom of the screen, so skip
                        \ the following instruction, leaving X at 191

 LDX #0                 \ Set X = 0

.LL107

 STX XX13               \ Set XX13 = X, so we have:
                        \
                        \   * XX13 = 0 if x2_hi = y2_hi = 0, y2_lo is on-screen
                        \
                        \   * XX13 = 191 if x2_hi or y2_hi are non-zero or y2_lo
                        \            is off the bottom of the screen
                        \
                        \ In other words, XX13 is 191 if (x2, y2) is off-screen,
                        \ otherwise it is 0

 LDA XX15+1             \ If one or both of x1_hi and y1_hi are non-zero, jump
 ORA XX15+3             \ to LL83
 BNE LL83

 LDA #Y*2-1             \ If y1_lo > the y-coordinate of the bottom of screen
 CMP XX15+2             \ then (x1, y1) is off the bottom of the screen, so jump
 BCC LL83               \ to LL83

                        \ If we get here, (x1, y1) is on-screen

 LDA XX13               \ If XX13 is non-zero, i.e. (x2, y2) is off-screen, jump
 BNE LL108              \ to LL108 to halve it before continuing at LL83

                        \ If we get here, the high bytes are all zero, which
                        \ means the x-coordinates are < 256 and therefore fit on
                        \ screen, and neither coordinate is off the bottom of
                        \ the screen. That means both coordinates are already on
                        \ screen, so we don't need to do any clipping, all we
                        \ need to do is move the low bytes into (X1, Y1) and
                        \ X2, Y2) and return

.LL146

                        \ If we get here then we have clipped our line to the
                        \ screen edge (if we had to clip it at all), so we move
                        \ the low bytes from (x1, y1) and (x2, y2) into (X1, Y1)
                        \ and (X2, Y2), remembering that they share locations
                        \ with XX15:
                        \
                        \   X1 = XX15
                        \   Y1 = XX15+1
                        \   X2 = XX15+2
                        \   Y2 = XX15+3
                        \
                        \ X1 already contains x1_lo, so now we do the rest

 LDA XX15+2             \ Set Y1 (aka XX15+1) = y1_lo
 STA XX15+1

 LDA XX15+4             \ Set X2 (aka XX15+2) = x2_lo
 STA XX15+2

 LDA XX12               \ Set Y2 (aka XX15+3) = y2_lo
 STA XX15+3

 CLC                    \ Clear the C flag as the clipped line fits on-screen

 RTS                    \ Return from the subroutine

.LL109

 SEC                    \ Set the C flag to indicate the clipped line does not
                        \ fit on-screen

 RTS                    \ Return from the subroutine

.LL108

 LSR XX13               \ If we get here then (x2, y2) is off-screen and XX13 is
                        \ 191, so shift XX13 right to halve it to 95

\ ******************************************************************************
\
\       Name: LL145 (Part 2 of 4)
\       Type: Subroutine
\   Category: Drawing lines
\    Summary: Clip line: Work out if any part of the line is on-screen
\  Deep dive: Line-clipping
\             Extended screen coordinates
\
\ ------------------------------------------------------------------------------
\
\ This part does a number of tests to see if the line is on or off the screen.
\
\ If we get here then at least one of (x1, y1) and (x2, y2) is off-screen, with
\ XX13 set as follows:
\
\   * 0   = (x1, y1) off-screen, (x2, y2) on-screen
\
\   * 95  = (x1, y1) on-screen,  (x2, y2) off-screen
\
\   * 191 = (x1, y1) off-screen, (x2, y2) off-screen
\
\ where "off-screen" is defined as having a non-zero high byte in one of the
\ coordinates, or in the case of y-coordinates, having a low byte > 191, the
\ y-coordinate of the bottom of the space view.
\
\ ******************************************************************************

.LL83

 LDA XX13               \ If XX13 < 128 then only one of the points is on-screen
 BPL LL115              \ so jump down to LL115 to skip the checks of whether
                        \ both points are in the strips to the right or bottom
                        \ of the screen

                        \ If we get here, both points are off-screen

 LDA XX15+1             \ If both x1_hi and x2_hi have bit 7 set, jump to LL109
 AND XX15+5             \ to return from the subroutine with the C flag set, as
 BMI LL109              \ the entire line is above the top of the screen

 LDA XX15+3             \ If both y1_hi and y2_hi have bit 7 set, jump to LL109
 AND XX12+1             \ to return from the subroutine with the C flag set, as
 BMI LL109              \ the entire line is to the left of the screen

 LDX XX15+1             \ Set A = X = x1_hi - 1
 DEX
 TXA

 LDX XX15+5             \ Set XX12+2 = x2_hi - 1
 DEX
 STX XX12+2

 ORA XX12+2             \ If neither (x1_hi - 1) or (x2_hi - 1) have bit 7 set,
 BPL LL109              \ jump to LL109 to return from the subroutine with the C
                        \ flag set, as the line doesn't fit on-screen

 LDA XX15+2             \ If y1_lo < y-coordinate of screen bottom, clear the C
 CMP #Y*2               \ flag, otherwise set it

 LDA XX15+3             \ Set XX12+2 = y1_hi - (1 - C), so:
 SBC #0                 \
 STA XX12+2             \  * Set XX12+2 = y1_hi - 1 if y1_lo is on-screen
                        \  * Set XX12+2 = y1_hi     otherwise
                        \
                        \ We do this subtraction because we are only interested
                        \ in trying to move the points up by a screen if that
                        \ might move the point into the space view portion of
                        \ the screen, i.e. if y1_lo is on-screen

 LDA XX12               \ If y2_lo < y-coordinate of screen bottom, clear the C
 CMP #Y*2               \ flag, otherwise set it

 LDA XX12+1             \ Set XX12+2 = y2_hi - (1 - C), so:
 SBC #0                 \
                        \  * Set XX12+1 = y2_hi - 1 if y2_lo is on-screen
                        \  * Set XX12+1 = y2_hi     otherwise
                        \
                        \ We do this subtraction because we are only interested
                        \ in trying to move the points up by a screen if that
                        \ might move the point into the space view portion of
                        \ the screen, i.e. if y1_lo is on-screen

 ORA XX12+2             \ If neither XX12+1 or XX12+2 have bit 7 set, jump to
 BPL LL109              \ LL109 to return from the subroutine with the C flag
                        \ set, as the line doesn't fit on-screen

\ ******************************************************************************
\
\       Name: LL145 (Part 3 of 4)
\       Type: Subroutine
\   Category: Drawing lines
\    Summary: Clip line: Calculate the line's gradient
\  Deep dive: Line-clipping
\             Extended screen coordinates
\
\ ******************************************************************************

.LL115

 TYA                    \ Store Y on the stack so we can preserve it through the
 PHA                    \ call to this subroutine

 LDA XX15+4             \ Set XX12+2 = x2_lo - x1_lo
 SEC
 SBC XX15
 STA XX12+2

 LDA XX15+5             \ Set XX12+3 = x2_hi - x1_hi
 SBC XX15+1
 STA XX12+3

 LDA XX12               \ Set XX12+4 = y2_lo - y1_lo
 SEC
 SBC XX15+2
 STA XX12+4

 LDA XX12+1             \ Set XX12+5 = y2_hi - y1_hi
 SBC XX15+3
 STA XX12+5

                        \ So we now have:
                        \
                        \   delta_x in XX12(3 2)
                        \   delta_y in XX12(5 4)
                        \
                        \ where the delta is (x1, y1) - (x2, y2))

 EOR XX12+3             \ Set S = the sign of delta_x * the sign of delta_y, so
 STA S                  \ if bit 7 of S is set, the deltas have different signs

 LDA XX12+5             \ If delta_y_hi is positive, jump down to LL110 to skip
 BPL LL110              \ the following

 LDA #0                 \ Otherwise flip the sign of delta_y to make it
 SEC                    \ positive, starting with the low bytes
 SBC XX12+4
 STA XX12+4

 LDA #0                 \ And then doing the high bytes, so now:
 SBC XX12+5             \
 STA XX12+5             \   XX12(5 4) = |delta_y|

.LL110

 LDA XX12+3             \ If delta_x_hi is positive, jump down to LL111 to skip
 BPL LL111              \ the following

 SEC                    \ Otherwise flip the sign of delta_x to make it
 LDA #0                 \ positive, starting with the low bytes
 SBC XX12+2
 STA XX12+2

 LDA #0                 \ And then doing the high bytes, so now:
 SBC XX12+3             \
                        \   (A XX12+2) = |delta_x|

.LL111

                        \ We now keep halving |delta_x| and |delta_y| until
                        \ both of them have zero in their high bytes

 TAX                    \ If |delta_x_hi| is non-zero, skip the following
 BNE LL112

 LDX XX12+5             \ If |delta_y_hi| = 0, jump down to LL113 (as both
 BEQ LL113              \ |delta_x_hi| and |delta_y_hi| are 0)

.LL112

 LSR A                  \ Halve the value of delta_x in (A XX12+2)
 ROR XX12+2

 LSR XX12+5             \ Halve the value of delta_y XX12(5 4)
 ROR XX12+4

 JMP LL111              \ Loop back to LL111

.LL113

                        \ By now, the high bytes of both |delta_x| and |delta_y|
                        \ are zero

 STX T                  \ We know that X = 0 as that's what we tested with a BEQ
                        \ above, so this sets T = 0

 LDA XX12+2             \ If delta_x_lo < delta_y_lo, so our line is more
 CMP XX12+4             \ vertical than horizontal, jump to LL114
 BCC LL114

                        \ If we get here then our line is more horizontal than
                        \ vertical, so it is a shallow slope

 STA Q                  \ Set Q = delta_x_lo

 LDA XX12+4             \ Set A = delta_y_lo

 JSR LL28               \ Call LL28 to calculate:
                        \
                        \   R = 256 * A / Q
                        \     = 256 * delta_y_lo / delta_x_lo

 JMP LL116              \ Jump to LL116, as we now have the line's gradient in R

.LL114

                        \ If we get here then our line is more vertical than
                        \ horizontal, so it is a steep slope

 LDA XX12+4             \ Set Q = delta_y_lo
 STA Q
 LDA XX12+2             \ Set A = delta_x_lo

 JSR LL28               \ Call LL28 to calculate:
                        \
                        \   R = 256 * A / Q
                        \     = 256 * delta_x_lo / delta_y_lo

 DEC T                  \ T was set to 0 above, so this sets T = &FF when our
                        \ line is steep

\ ******************************************************************************
\
\       Name: LL145 (Part 4 of 4)
\       Type: Subroutine
\   Category: Drawing lines
\    Summary: Clip line: Call the routine in LL188 to do the actual clipping
\  Deep dive: Line-clipping
\             Extended screen coordinates
\
\ ------------------------------------------------------------------------------
\
\ This part sets things up to call the routine in LL188, which does the actual
\ clipping.
\
\ If we get here, then R has been set to the gradient of the line (x1, y1) to
\ (x2, y2), with T indicating the gradient of slope:
\
\   * 0   = shallow slope (more horizontal than vertical)
\
\   * &FF = steep slope (more vertical than horizontal)
\
\ and XX13 has been set as follows:
\
\   * 0   = (x1, y1) off-screen, (x2, y2) on-screen
\
\   * 95  = (x1, y1) on-screen,  (x2, y2) off-screen
\
\   * 191 = (x1, y1) off-screen, (x2, y2) off-screen
\
\ ******************************************************************************

.LL116

 LDA R                  \ Store the gradient in XX12+2
 STA XX12+2

 LDA S                  \ Store the type of slope in XX12+3, bit 7 clear means
 STA XX12+3             \ top left to bottom right, bit 7 set means top right to
                        \ bottom left

 LDA XX13               \ If XX13 = 0, skip the following instruction
 BEQ LL138

 BPL LLX117             \ If XX13 is positive, it must be 95. This means
                        \ (x1, y1) is on-screen but (x2, y2) isn't, so we jump
                        \ to LLX117 to swap the (x1, y1) and (x2, y2)
                        \ coordinates around before doing the actual clipping,
                        \ because we need to clip (x2, y2) but the clipping
                        \ routine at LL118 only clips (x1, y1)

.LL138

                        \ If we get here, XX13 = 0 or 191, so (x1, y1) is
                        \ off-screen and needs clipping

 JSR LL118              \ Call LL118 to move (x1, y1) along the line onto the
                        \ screen, i.e. clip the line at the (x1, y1) end

 LDA XX13               \ If XX13 = 0, i.e. (x2, y2) is on-screen, jump down to
 BPL LL124              \ LL124 to return with a successfully clipped line

.LL117

                        \ If we get here, XX13 = 191 (both coordinates are
                        \ off-screen)

 LDA XX15+1             \ If either of x1_hi or y1_hi are non-zero, jump to
 ORA XX15+3             \ LL137 to return from the subroutine with the C flag
 BNE LL137              \ set, as the line doesn't fit on-screen

 LDA XX15+2             \ If y1_lo > y-coordinate of the bottom of the screen
 CMP #Y*2               \ jump to LL137 to return from the subroutine with the
 BCS LL137              \ C flag set, as the line doesn't fit on-screen

.LLX117

                        \ If we get here, XX13 = 95 or 191, and in both cases
                        \ (x2, y2) is off-screen, so we now need to swap the
                        \ (x1, y1) and (x2, y2) coordinates around before doing
                        \ the actual clipping, because we need to clip (x2, y2)
                        \ but the clipping routine at LL118 only clips (x1, y1)

 LDX XX15               \ Swap x1_lo = x2_lo
 LDA XX15+4
 STA XX15
 STX XX15+4

 LDA XX15+5             \ Swap x2_lo = x1_lo
 LDX XX15+1
 STX XX15+5
 STA XX15+1

 LDX XX15+2             \ Swap y1_lo = y2_lo
 LDA XX12
 STA XX15+2
 STX XX12

 LDA XX12+1             \ Swap y2_lo = y1_lo
 LDX XX15+3
 STX XX12+1
 STA XX15+3

 JSR LL118              \ Call LL118 to move (x1, y1) along the line onto the
                        \ screen, i.e. clip the line at the (x1, y1) end

 DEC SWAP               \ Set SWAP = &FF to indicate that we just clipped the
                        \ line at the (x2, y2) end by swapping the coordinates
                        \ (the DEC does this as we set SWAP to 0 at the start of
                        \ this subroutine)

.LL124

 PLA                    \ Restore Y from the stack so it gets preserved through
 TAY                    \ the call to this subroutine

 JMP LL146              \ Jump up to LL146 to move the low bytes of (x1, y1) and
                        \ (x2, y2) into (X1, Y1) and (X2, Y2), and return from
                        \ the subroutine with a successfully clipped line

.LL137

 PLA                    \ Restore Y from the stack so it gets preserved through
 TAY                    \ the call to this subroutine

 SEC                    \ Set the C flag to indicate the clipped line does not
                        \ fit on-screen

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL9 (Part 12 of 12)
\       Type: Subroutine
\   Category: Drawing ships
\    Summary: Draw ship: Draw all the visible edges from the ship line heap
\  Deep dive: Drawing ships
\
\ ------------------------------------------------------------------------------
\
\ This part draws the lines in the ship line heap, which is used both to draw
\ the ship, and to remove it from the screen.
\
\ ******************************************************************************

.LL155

 LDY LSNUM              \ Set Y to the offset in the line heap LSNUM

.LL27

 CPY LSNUM2             \ If Y >= LSNUM2, jump to LLEX to return from the ship
 BCS LLEX               \ drawing routine, because the index in Y is greater
                        \ than the size of the existing ship line heap, which
                        \ means we have alrady erased all the old ships lines
                        \ when drawing the new ship

                        \ If we get here then Y < LSNUM2, which means Y is
                        \ pointing to an on-screen line from the old ship that
                        \ we need to erase

 LDA (XX19),Y           \ Fetch the X1 line coordinate from the heap and store
 STA XX15               \ it in XX15

 INY                    \ Increment the heap pointer

 LDA (XX19),Y           \ Fetch the Y1 line coordinate from the heap and store
 STA XX15+1             \ it in XX15+1

 INY                    \ Increment the heap pointer

 LDA (XX19),Y           \ Fetch the X2 line coordinate from the heap and store
 STA XX15+2             \ it in XX15+2

 INY                    \ Increment the heap pointer

 LDA (XX19),Y           \ Fetch the Y2 line coordinate from the heap and store
 STA XX15+3             \ it in XX15+3

 JSR LL30               \ Draw a line from (X1, Y1) to (X2, Y2) to erase it from
                        \ the screen

 INY                    \ Increment the heap pointer

 JMP LL27               \ Loop back to LL27 to draw (i.e. erase) the next line
                        \ from the heap

.LLEX

 LDA LSNUM              \ Store LSNUM in the first byte of the ship line heap
 LDY #0
 STA (XX19),Y

.LL82

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LSPUT
\       Type: Subroutine
\   Category: Drawing lines
\    Summary: Draw a ship line using smooth animation, by drawing the ship's new
\             line and erasing the corresponding old line from the screen
\
\ ------------------------------------------------------------------------------
\
\ This routine implements smoother ship animation by erasing and redrawing each
\ individual line in the ship, rather than the approach in the other Acornsoft
\ versions of the game, which erase the entire existing ship before drawing the
\ new one.
\
\ Here's the new approach in this routine:
\
\   * Draw the new line
\
\   * Fetch the corresponding existing line (in position LSNUM) from the heap
\
\   * Store the new line in the heap at this position, replacing the old one
\
\   * If the existing line we just took from the heap is on-screen, erase it
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   LSNUM               The offset within the line heap where we add the new
\                       line's coordinates
\
\   X1                  The screen x-coordinate of the start of the line to add
\                       to the ship line heap
\
\   Y1                  The screen y-coordinate of the start of the line to add
\                       to the ship line heap
\
\   X2                  The screen x-coordinate of the end of the line to add
\                       to the ship line heap
\
\   Y2                  The screen y-coordinate of the end of the line to add
\                       to the ship line heap
\
\   XX19(1 0)           XX19(1 0) shares its location with INWK(34 33), which
\                       contains the ship line heap address pointer
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   LSNUM               The offset of the next line in the line heap
\
\ ******************************************************************************

.LSPUT

 LDY LSNUM              \ Set Y = LSNUM, to get the offset within the ship line
                        \ heap where we want to insert our new line

 CPY LSNUM2             \ Compare LSNUM and LSNUM2 and store the flags on the
 PHP                    \ stack so we can retrieve them later

 LDX #3                 \ We now want to copy the line coordinates (X1, Y1) and
                        \ (X2, Y2) to XX12...XX12+3, so set a counter to copy
                        \ 4 bytes

.LLXL

 LDA X1,X               \ Copy the X-th byte of X1/Y1/X2/Y2 to the X-th byte of
 STA XX12,X             \ XX12

 DEX                    \ Decrement the loop counter

 BPL LLXL               \ Loop back until we have copied all four bytes

 JSR LL30               \ Draw a line from (X1, Y1) to (X2, Y2)

 LDA (XX19),Y           \ Set X1 to the Y-th coordinate on the ship line heap,
 STA X1                 \ i.e. one we are replacing in the heap

 LDA XX12               \ Replace it with the X1 coordinate in XX12
 STA (XX19),Y

 INY                    \ Increment the index to point to the Y1 coordinate

 LDA (XX19),Y           \ Set Y1 to the Y-th coordinate on the ship line heap,
 STA Y1                 \ i.e. one we are replacing in the heap

 LDA XX12+1             \ Replace it with the Y1 coordinate in XX12+1
 STA (XX19),Y

 INY                    \ Increment the index to point to the X2 coordinate

 LDA (XX19),Y           \ Set X1 to the Y-th coordinate on the ship line heap,
 STA X2

 LDA XX12+2             \ Replace it with the X2 coordinate in XX12+2
 STA (XX19),Y

 INY                    \ Increment the index to point to the Y2 coordinate

 LDA (XX19),Y           \ Set Y2 to the Y-th coordinate on the ship line heap,
 STA Y2

 LDA XX12+3             \ Replace it with the Y2 coordinate in XX12+3
 STA (XX19),Y

 INY                    \ Increment the index to point to the next coordinate
 STY LSNUM              \ and store the updated index in LSNUM

 PLP                    \ Restore the result of the comparison above, so if the
 BCS LL82               \ original value of LSNUM >= LSNUM2, then we have
                        \ alreadyredrawn all the lines from the old ship's line
                        \ heap, so return from the subroutine (as LL82 contains
                        \ an RTS)

 JMP LL30               \ Otherwise there are still more lines to erase from the
                        \ old ship on-screen, so the coordinates in (X1, Y1) and
                        \ (X2, Y2) that we just pulled from the ship line heap
                        \ point to a line that is still on-screen, so call LL30
                        \ to draw this line and erase it from the screen,
                        \ returning from the subroutine using a tail call

\ ******************************************************************************
\
\       Name: LL118
\       Type: Subroutine
\   Category: Drawing lines
\    Summary: Move a point along a line until it is on-screen
\  Deep dive: Line-clipping
\
\ ------------------------------------------------------------------------------
\
\ Given a point (x1, y1), a gradient and a direction of slope, move the point
\ along the line until it is on-screen, so this effectively clips the (x1, y1)
\ end of a line to be on the screen.
\
\ See the deep dive on "Line-clipping" for more details.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   XX15(1 0)           x1 as a 16-bit coordinate (x1_hi x1_lo)
\
\   XX15(3 2)           y1 as a 16-bit coordinate (y1_hi y1_lo)
\
\   XX12+2              The line's gradient * 256 (so 1.0 = 256)
\
\   XX12+3              The direction of slope:
\
\                         * Positive (bit 7 clear) = top left to bottom right
\
\                         * Negative (bit 7 set) = top right to bottom left
\
\   T                   The gradient of slope:
\
\                         * 0 if it's a shallow slope
\
\                         * &FF if it's a steep slope
\
\ ------------------------------------------------------------------------------
\
\ Returns:
\
\   XX15                x1 as an 8-bit coordinate
\
\   XX15+2              y1 as an 8-bit coordinate
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   LL118-1             Contains an RTS
\
\ ******************************************************************************

.LL118

 LDA XX15+1             \ If x1_hi is positive, jump down to LL119 to skip the
 BPL LL119              \ following

 STA S                  \ Otherwise x1_hi is negative, i.e. off the left of the
                        \ screen, so set S = x1_hi

 JSR LL120              \ Call LL120 to calculate:
                        \
                        \   (Y X) = (S x1_lo) * XX12+2      if T = 0
                        \         = x1 * gradient
                        \
                        \   (Y X) = (S x1_lo) / XX12+2      if T <> 0
                        \         = x1 / gradient
                        \
                        \ with the sign of (Y X) set to the opposite of the
                        \ line's direction of slope

 TXA                    \ Set y1 = y1 + (Y X)
 CLC                    \
 ADC XX15+2             \ starting with the low bytes
 STA XX15+2

 TYA                    \ And then adding the high bytes
 ADC XX15+3
 STA XX15+3

 LDA #0                 \ Set x1 = 0
 STA XX15
 STA XX15+1

 TAX                    \ Set X = 0 so the next instruction becomes a JMP

.LL119

 BEQ LL134              \ If x1_hi = 0 then jump down to LL134 to skip the
                        \ following, as the x-coordinate is already on-screen
                        \ (as 0 <= (x_hi x_lo) <= 255)

 STA S                  \ Otherwise x1_hi is positive, i.e. x1 >= 256 and off
 DEC S                  \ the right side of the screen, so set S = x1_hi - 1

 JSR LL120              \ Call LL120 to calculate:
                        \
                        \   (Y X) = (S x1_lo) * XX12+2      if T = 0
                        \         = (x1 - 256) * gradient
                        \
                        \   (Y X) = (S x1_lo) / XX12+2      if T <> 0
                        \         = (x1 - 256) / gradient
                        \
                        \ with the sign of (Y X) set to the opposite of the
                        \ line's direction of slope

 TXA                    \ Set y1 = y1 + (Y X)
 CLC                    \
 ADC XX15+2             \ starting with the low bytes
 STA XX15+2

 TYA                    \ And then adding the high bytes
 ADC XX15+3
 STA XX15+3

 LDX #255               \ Set x1 = 255
 STX XX15
 INX
 STX XX15+1

.LL134

                        \ We have moved the point so the x-coordinate is on
                        \ screen (i.e. in the range 0-255), so now for the
                        \ y-coordinate

 LDA XX15+3             \ If y1_hi is positive, jump down to LL119 to skip
 BPL LL135              \ the following

 STA S                  \ Otherwise y1_hi is negative, i.e. off the top of the
                        \ screen, so set S = y1_hi

 LDA XX15+2             \ Set R = y1_lo
 STA R

 JSR LL123              \ Call LL123 to calculate:
                        \
                        \   (Y X) = (S R) / XX12+2      if T = 0
                        \         = y1 / gradient
                        \
                        \   (Y X) = (S R) * XX12+2      if T <> 0
                        \         = y1 * gradient
                        \
                        \ with the sign of (Y X) set to the opposite of the
                        \ line's direction of slope

 TXA                    \ Set x1 = x1 + (Y X)
 CLC                    \
 ADC XX15               \ starting with the low bytes
 STA XX15

 TYA                    \ And then adding the high bytes
 ADC XX15+1
 STA XX15+1

 LDA #0                 \ Set y1 = 0
 STA XX15+2
 STA XX15+3

.LL135

 LDA XX15+2             \ Set (S R) = (y1_hi y1_lo) - screen height
 SEC                    \
 SBC #Y*2               \ starting with the low bytes
 STA R

 LDA XX15+3             \ And then subtracting the high bytes
 SBC #0
 STA S

 BCC LL136              \ If the subtraction underflowed, i.e. if y1 < screen
                        \ height, then y1 is already on-screen, so jump to LL136
                        \ to return from the subroutine, as we are done

.LL139

                        \ If we get here then y1 >= screen height, i.e. off the
                        \ bottom of the screen

 JSR LL123              \ Call LL123 to calculate:
                        \
                        \   (Y X) = (S R) / XX12+2      if T = 0
                        \         = (y1 - screen height) / gradient
                        \
                        \   (Y X) = (S R) * XX12+2      if T <> 0
                        \         = (y1 - screen height) * gradient
                        \
                        \ with the sign of (Y X) set to the opposite of the
                        \ line's direction of slope

 TXA                    \ Set x1 = x1 + (Y X)
 CLC                    \
 ADC XX15               \ starting with the low bytes
 STA XX15

 TYA                    \ And then adding the high bytes
 ADC XX15+1
 STA XX15+1

 LDA #Y*2-1             \ Set y1 = 2 * #Y - 1. The constant #Y is 96, the
 STA XX15+2             \ y-coordinate of the mid-point of the space view, so
 LDA #0                 \ this sets Y2 to 191, the y-coordinate of the bottom
 STA XX15+3             \ pixel row of the space view

.LL136

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL120
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (Y X) = (S x1_lo) * XX12+2 or (S x1_lo) / XX12+2
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following:
\
\   * If T = 0, this is a shallow slope, so calculate (Y X) = (S x1_lo) * XX12+2
\
\   * If T <> 0, this is a steep slope, so calculate (Y X) = (S x1_lo) / XX12+2
\
\ giving (Y X) the opposite sign to the slope direction in XX12+3.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   T                   The gradient of slope:
\
\                         * 0 if it's a shallow slope
\
\                         * &FF if it's a steep slope
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   LL122               Calculate (Y X) = (S R) * Q and set the sign to the
\                       opposite of the top byte on the stack
\
\ ******************************************************************************

.LL120

 LDA XX15               \ Set R = x1_lo
 STA R

 JSR LL129              \ Call LL129 to do the following:
                        \
                        \   Q = XX12+2
                        \     = line gradient
                        \
                        \   A = S EOR XX12+3
                        \     = S EOR slope direction
                        \
                        \   (S R) = |S R|
                        \
                        \ So A contains the sign of S * slope direction

 PHA                    \ Store A on the stack so we can use it later

 LDX T                  \ If T is non-zero, then it's a steep slope, so jump
 BNE LL121              \ down to LL121 to calculate this instead:
                        \
                        \   (Y X) = (S R) / Q

.LL122

                        \ The following calculates:
                        \
                        \   (Y X) = (S R) * Q
                        \
                        \ using the same shift-and-add algorithm that's
                        \ documented in MULT1

 LDA #0                 \ Set A = 0

 TAX                    \ Set (Y X) = 0 so we can start building the answer here
 TAY

 LSR S                  \ Shift (S R) to the right, so we extract bit 0 of (S R)
 ROR R                  \ into the C flag

 ASL Q                  \ Shift Q to the left, catching bit 7 in the C flag

 BCC LL126              \ If C (i.e. the next bit from Q) is clear, do not do
                        \ the addition for this bit of Q, and instead skip to
                        \ LL126 to just do the shifts

.LL125

 TXA                    \ Set (Y X) = (Y X) + (S R)
 CLC                    \
 ADC R                  \ starting with the low bytes
 TAX

 TYA                    \ And then doing the high bytes
 ADC S
 TAY

.LL126

 LSR S                  \ Shift (S R) to the right
 ROR R

 ASL Q                  \ Shift Q to the left, catching bit 7 in the C flag

 BCS LL125              \ If C (i.e. the next bit from Q) is set, loop back to
                        \ LL125 to do the addition for this bit of Q

 BNE LL126              \ If Q has not yet run out of set bits, loop back to
                        \ LL126 to do the "shift" part of shift-and-add until
                        \ we have done additions for all the set bits in Q, to
                        \ give us our multiplication result

 PLA                    \ Restore A, which we calculated above, from the stack

 BPL LL133              \ If A is positive jump to LL133 to negate (Y X) and
                        \ return from the subroutine using a tail call

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL123
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate (Y X) = (S R) / XX12+2 or (S R) * XX12+2
\
\ ------------------------------------------------------------------------------
\
\ Calculate the following:
\
\   * If T = 0, this is a shallow slope, so calculate (Y X) = (S R) / XX12+2
\
\   * If T <> 0, this is a steep slope, so calculate (Y X) = (S R) * XX12+2
\
\ giving (Y X) the opposite sign to the slope direction in XX12+3.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   XX12+2              The line's gradient * 256 (so 1.0 = 256)
\
\   XX12+3              The direction of slope:
\
\                         * Bit 7 clear means top left to bottom right
\
\                         * Bit 7 set means top right to bottom left
\
\   T                   The gradient of slope:
\
\                         * 0 if it's a shallow slope
\
\                         * &FF if it's a steep slope
\
\ ------------------------------------------------------------------------------
\
\ Other entry points:
\
\   LL121               Calculate (Y X) = (S R) / Q and set the sign to the
\                       opposite of the top byte on the stack
\
\   LL133               Negate (Y X) and return from the subroutine
\
\   LL128               Contains an RTS
\
\ ******************************************************************************

.LL123

 JSR LL129              \ Call LL129 to do the following:
                        \
                        \   Q = XX12+2
                        \     = line gradient
                        \
                        \   A = S EOR XX12+3
                        \     = S EOR slope direction
                        \
                        \   (S R) = |S R|
                        \
                        \ So A contains the sign of S * slope direction

 PHA                    \ Store A on the stack so we can use it later

 LDX T                  \ If T is non-zero, then it's a steep slope, so jump up
 BNE LL122              \ to LL122 to calculate this instead:
                        \
                        \   (Y X) = (S R) * Q

.LL121

                        \ The following calculates:
                        \
                        \   (Y X) = (S R) / Q
                        \
                        \ using the same shift-and-subtract algorithm that's
                        \ documented in TIS2

 LDA #%11111111         \ Set Y = %11111111
 TAY

 ASL A                  \ Set X = %11111110
 TAX

                        \ This sets (Y X) = %1111111111111110, so we can rotate
                        \ through 15 loop iterations, getting a 1 each time, and
                        \ then getting a 0 on the 16th iteration... and we can
                        \ also use it to catch our result bits into bit 0 each
                        \ time

.LL130

 ASL R                  \ Shift (S R) to the left
 ROL S

 LDA S                  \ Set A = S

 BCS LL131              \ If bit 7 of S was set, then jump straight to the
                        \ subtraction

 CMP Q                  \ If A < Q (i.e. S < Q), skip the following subtractions
 BCC LL132

.LL131

 SBC Q                  \ A >= Q (i.e. S >= Q) so set:
 STA S                  \
                        \   S = (A R) - Q
                        \     = (S R) - Q
                        \
                        \ starting with the low bytes (we know the C flag is
                        \ set so the subtraction will be correct)

 LDA R                  \ And then doing the high bytes
 SBC #0
 STA R

 SEC                    \ Set the C flag to rotate into the result in (Y X)

.LL132

 TXA                    \ Rotate the counter in (Y X) to the left, and catch the
 ROL A                  \ result bit into bit 0 (which will be a 0 if we didn't
 TAX                    \ do the subtraction, or 1 if we did)
 TYA
 ROL A
 TAY

 BCS LL130              \ If we still have set bits in (Y X), loop back to LL130
                        \ to do the next iteration of 15, until we have done the
                        \ whole division

 PLA                    \ Restore A, which we calculated above, from the stack

 BMI LL128              \ If A is negative jump to LL128 to return from the
                        \ subroutine with (Y X) as is

.LL133

 TXA                    \ Otherwise negate (Y X) using two's complement by first
 EOR #%11111111         \ setting the low byte to ~X + 1
 ADC #1                 \
 TAX                    \ The addition works as we know the C flag is clear from
                        \ when we passed through the BCS above

 TYA                    \ Then set the high byte to ~Y + C
 EOR #%11111111
 ADC #0
 TAY

.LL128

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: LL129
\       Type: Subroutine
\   Category: Maths (Arithmetic)
\    Summary: Calculate Q = XX12+2, A = S EOR XX12+3 and (S R) = |S R|
\
\ ------------------------------------------------------------------------------
\
\ Do the following, in this order:
\
\   Q = XX12+2
\
\   A = S EOR XX12+3
\
\   (S R) = |S R|
\
\ This sets up the variables required above to calculate (S R) / XX12+2 and give
\ the result the opposite sign to XX13+3.
\
\ ******************************************************************************

.LL129

 LDX XX12+2             \ Set Q = XX12+2
 STX Q

 LDA S                  \ If S is positive, jump to LL127
 BPL LL127

 LDA #0                 \ Otherwise set R = -R
 SEC
 SBC R
 STA R

 LDA S                  \ Push S onto the stack
 PHA

 EOR #%11111111         \ Set S = ~S + 1 + C
 ADC #0
 STA S

 PLA                    \ Pull the original, negative S from the stack into A

.LL127

 EOR XX12+3             \ Set A = original argument S EOR'd with XX12+3

 RTS                    \ Return from the subroutine

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

 STA (V),Y              \ Poke the byte into screen memory in V(1 0)

 INY                    \ Increment the index

 BPL pzer1              \ Loop back to print the next character

.pzer2

 RTS                    \ Return from the subroutine

\ ******************************************************************************
\
\       Name: titleText
\       Type: Variable
\   Category: Teletext Elite
\    Summary: Loading screen title
\
\ ******************************************************************************

.titleText

 EQUS "Elite Over Econet"
 EQUB 0

\ ******************************************************************************
\
\       Name: XX21
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprints lookup table for the ship hangar
\  Deep dive: Ship blueprints in the disc version
\
\ ******************************************************************************

.XX21

 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW SHIP_CANISTER     \ OIL  =  5 = Cargo canister
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW SHIP_SHUTTLE      \ SHU  =  9 = Shuttle
 EQUW SHIP_TRANSPORTER  \        10 = Transporter
 EQUW SHIP_COBRA_MK_3   \ CYL  = 11 = Cobra Mk III
 EQUW SHIP_PYTHON       \        12 = Python
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW SHIP_VIPER        \ COPS = 16 = Viper
 EQUW 0
 EQUW 0
 EQUW SHIP_KRAIT        \ KRA  = 19 = Krait
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW 0
 EQUW SHIP_CONSTRICTOR  \ CON  = 31 = Constrictor

\ ******************************************************************************
\
\       Name: E%
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprints default NEWB flags for the ship hangar
\  Deep dive: Ship blueprints
\             Advanced tactics with the NEWB flags
\
\ ******************************************************************************

.E%

 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB %00000000         \ Cargo canister
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB %00100001         \ Shuttle                               Trader, innocent
 EQUB %01100001         \ Transporter                      Trader, innocent, cop
 EQUB %10100000         \ Cobra Mk III                      Innocent, escape pod
 EQUB %10100000         \ Python                            Innocent, escape pod
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB %11000010         \ Viper                   Bounty hunter, cop, escape pod
 EQUB 0
 EQUB 0
 EQUB %10001100         \ Krait                      Hostile, pirate, escape pod
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB 0
 EQUB %10001100         \ Constrictor                Hostile, pirate, escape pod

\ ******************************************************************************
\
\       Name: VERTEX
\       Type: Macro
\   Category: Drawing ships
\    Summary: Macro definition for adding vertices to ship blueprints
\  Deep dive: Ship blueprints
\
\ ------------------------------------------------------------------------------
\
\ The following macro is used to build the ship blueprints:
\
\   VERTEX x, y, z, face1, face2, face3, face4, visibility
\
\ See the deep dive on "Ship blueprints" for details of how vertices are stored
\ in the ship blueprints, and the deep dive on "Drawing ships" for information
\ on how vertices are used to draw 3D wireframe ships.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   x                   The vertex's x-coordinate
\
\   y                   The vertex's y-coordinate
\
\   z                   The vertex's z-coordinate
\
\   face1               The number of face 1 associated with this vertex
\
\   face2               The number of face 2 associated with this vertex
\
\   face3               The number of face 3 associated with this vertex
\
\   face4               The number of face 4 associated with this vertex
\
\   visibility          The visibility distance, beyond which the vertex is not
\                       shown
\
\ ******************************************************************************

MACRO VERTEX x, y, z, face1, face2, face3, face4, visibility

 IF x < 0
  s_x = 1 << 7
 ELSE
  s_x = 0
 ENDIF

 IF y < 0
  s_y = 1 << 6
 ELSE
  s_y = 0
 ENDIF

 IF z < 0
  s_z = 1 << 5
 ELSE
  s_z = 0
 ENDIF

 s = s_x + s_y + s_z + visibility
 f1 = face1 + (face2 << 4)
 f2 = face3 + (face4 << 4)
 ax = ABS(x)
 ay = ABS(y)
 az = ABS(z)

 EQUB ax, ay, az, s, f1, f2

ENDMACRO

\ ******************************************************************************
\
\       Name: EDGE
\       Type: Macro
\   Category: Drawing ships
\    Summary: Macro definition for adding edges to ship blueprints
\  Deep dive: Ship blueprints
\
\ ------------------------------------------------------------------------------
\
\ The following macro is used to build the ship blueprints:
\
\   EDGE vertex1, vertex2, face1, face2, visibility
\
\ See the deep dive on "Ship blueprints" for details of how edges are stored
\ in the ship blueprints, and the deep dive on "Drawing ships" for information
\ on how edges are used to draw 3D wireframe ships.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   vertex1             The number of the vertex at the start of the edge
\
\   vertex1             The number of the vertex at the end of the edge
\
\   face1               The number of face 1 associated with this edge
\
\   face2               The number of face 2 associated with this edge
\
\   visibility          The visibility distance, beyond which the edge is not
\                       shown
\
\ ******************************************************************************

MACRO EDGE vertex1, vertex2, face1, face2, visibility

 f = face1 + (face2 << 4)
 EQUB visibility, f, vertex1 << 2, vertex2 << 2

ENDMACRO

\ ******************************************************************************
\
\       Name: FACE
\       Type: Macro
\   Category: Drawing ships
\    Summary: Macro definition for adding faces to ship blueprints
\  Deep dive: Ship blueprints
\
\ ------------------------------------------------------------------------------
\
\ The following macro is used to build the ship blueprints:
\
\   FACE normal_x, normal_y, normal_z, visibility
\
\ See the deep dive on "Ship blueprints" for details of how faces are stored
\ in the ship blueprints, and the deep dive on "Drawing ships" for information
\ on how faces are used to draw 3D wireframe ships.
\
\ ------------------------------------------------------------------------------
\
\ Arguments:
\
\   normal_x            The face normal's x-coordinate
\
\   normal_y            The face normal's y-coordinate
\
\   normal_z            The face normal's z-coordinate
\
\   visibility          The visibility distance, beyond which the edge is always
\                       shown
\
\ ******************************************************************************

MACRO FACE normal_x, normal_y, normal_z, visibility

 IF normal_x < 0
  s_x = 1 << 7
 ELSE
  s_x = 0
 ENDIF

 IF normal_y < 0
  s_y = 1 << 6
 ELSE
  s_y = 0
 ENDIF

 IF normal_z < 0
  s_z = 1 << 5
 ELSE
  s_z = 0
 ENDIF

 s = s_x + s_y + s_z + visibility
 ax = ABS(normal_x)
 ay = ABS(normal_y)
 az = ABS(normal_z)

 EQUB s, ax, ay, az

ENDMACRO

\ ******************************************************************************
\
\       Name: SHIP_CANISTER
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprint for a cargo canister
\  Deep dive: Ship blueprints
\
\ ******************************************************************************

.SHIP_CANISTER

 EQUB 0                 \ Max. canisters on demise = 0
 EQUW 20 * 20           \ Targetable area          = 20 * 20

 EQUB LO(SHIP_CANISTER_EDGES - SHIP_CANISTER)      \ Edges data offset (low)
 EQUB LO(SHIP_CANISTER_FACES - SHIP_CANISTER)      \ Faces data offset (low)

 EQUB 49                \ Max. edge count          = (49 - 1) / 4 = 12
 EQUB 0                 \ Gun vertex               = 0
 EQUB 18                \ Explosion count          = 3, as (4 * n) + 6 = 18
 EQUB 60                \ Number of vertices       = 60 / 6 = 10
 EQUB 15                \ Number of edges          = 15
 EQUW 0                 \ Bounty                   = 0
 EQUB 28                \ Number of faces          = 28 / 4 = 7
 EQUB 12                \ Visibility distance      = 12
 EQUB 17                \ Max. energy              = 17
 EQUB 15                \ Max. speed               = 15

 EQUB HI(SHIP_CANISTER_EDGES - SHIP_CANISTER)      \ Edges data offset (high)
 EQUB HI(SHIP_CANISTER_FACES - SHIP_CANISTER)      \ Faces data offset (high)

 EQUB 2                 \ Normals are scaled by    = 2^2 = 4
 EQUB %00000000         \ Laser power              = 0
                        \ Missiles                 = 0

.SHIP_CANISTER_VERTICES

      \    x,    y,    z, face1, face2, face3, face4, visibility
 VERTEX   24,   16,    0,     0,      1,    5,     5,         31    \ Vertex 0
 VERTEX   24,    5,   15,     0,      1,    2,     2,         31    \ Vertex 1
 VERTEX   24,  -13,    9,     0,      2,    3,     3,         31    \ Vertex 2
 VERTEX   24,  -13,   -9,     0,      3,    4,     4,         31    \ Vertex 3
 VERTEX   24,    5,  -15,     0,      4,    5,     5,         31    \ Vertex 4
 VERTEX  -24,   16,    0,     1,      5,    6,     6,         31    \ Vertex 5
 VERTEX  -24,    5,   15,     1,      2,    6,     6,         31    \ Vertex 6
 VERTEX  -24,  -13,    9,     2,      3,    6,     6,         31    \ Vertex 7
 VERTEX  -24,  -13,   -9,     3,      4,    6,     6,         31    \ Vertex 8
 VERTEX  -24,    5,  -15,     4,      5,    6,     6,         31    \ Vertex 9

.SHIP_CANISTER_EDGES

    \ vertex1, vertex2, face1, face2, visibility
 EDGE       0,       1,     0,     1,         31    \ Edge 0
 EDGE       1,       2,     0,     2,         31    \ Edge 1
 EDGE       2,       3,     0,     3,         31    \ Edge 2
 EDGE       3,       4,     0,     4,         31    \ Edge 3
 EDGE       0,       4,     0,     5,         31    \ Edge 4
 EDGE       0,       5,     1,     5,         31    \ Edge 5
 EDGE       1,       6,     1,     2,         31    \ Edge 6
 EDGE       2,       7,     2,     3,         31    \ Edge 7
 EDGE       3,       8,     3,     4,         31    \ Edge 8
 EDGE       4,       9,     4,     5,         31    \ Edge 9
 EDGE       5,       6,     1,     6,         31    \ Edge 10
 EDGE       6,       7,     2,     6,         31    \ Edge 11
 EDGE       7,       8,     3,     6,         31    \ Edge 12
 EDGE       8,       9,     4,     6,         31    \ Edge 13
 EDGE       9,       5,     5,     6,         31    \ Edge 14

.SHIP_CANISTER_FACES

    \ normal_x, normal_y, normal_z, visibility
 FACE       96,        0,        0,         31    \ Face 0
 FACE        0,       41,       30,         31    \ Face 1
 FACE        0,      -18,       48,         31    \ Face 2
 FACE        0,      -51,        0,         31    \ Face 3
 FACE        0,      -18,      -48,         31    \ Face 4
 FACE        0,       41,      -30,         31    \ Face 5
 FACE      -96,        0,        0,         31    \ Face 6

\ ******************************************************************************
\
\       Name: SHIP_SHUTTLE
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprint for a Shuttle
\  Deep dive: Ship blueprints
\
\ ******************************************************************************

.SHIP_SHUTTLE

 EQUB 15                \ Max. canisters on demise = 15
 EQUW 50 * 50           \ Targetable area          = 50 * 50

 EQUB LO(SHIP_SHUTTLE_EDGES - SHIP_SHUTTLE)        \ Edges data offset (low)
 EQUB LO(SHIP_SHUTTLE_FACES - SHIP_SHUTTLE)        \ Faces data offset (low)

 EQUB 109               \ Max. edge count          = (109 - 1) / 4 = 27
 EQUB 0                 \ Gun vertex               = 0
 EQUB 38                \ Explosion count          = 8, as (4 * n) + 6 = 38
 EQUB 114               \ Number of vertices       = 114 / 6 = 19
 EQUB 30                \ Number of edges          = 30
 EQUW 0                 \ Bounty                   = 0
 EQUB 52                \ Number of faces          = 52 / 4 = 13
 EQUB 22                \ Visibility distance      = 22
 EQUB 32                \ Max. energy              = 32
 EQUB 8                 \ Max. speed               = 8

 EQUB HI(SHIP_SHUTTLE_EDGES - SHIP_SHUTTLE)        \ Edges data offset (high)
 EQUB HI(SHIP_SHUTTLE_FACES - SHIP_SHUTTLE)        \ Faces data offset (high)

 EQUB 2                 \ Normals are scaled by    = 2^2 = 4
 EQUB %00000000         \ Laser power              = 0
                        \ Missiles                 = 0

.SHIP_SHUTTLE_VERTICES

      \    x,    y,    z, face1, face2, face3, face4, visibility
 VERTEX    0,  -35,   47,    15,    15,    15,    15,         31     \ Vertex 0
 VERTEX  -35,    0,   47,    15,    15,    15,    15,         31     \ Vertex 1
 VERTEX    0,   35,   47,    15,    15,    15,    15,         31     \ Vertex 2
 VERTEX   35,    0,   47,    15,    15,    15,    15,         31     \ Vertex 3
 VERTEX  -40,  -40,  -53,     2,     1,     9,     3,         31     \ Vertex 4
 VERTEX  -40,   40,  -53,     4,     3,     9,     5,         31     \ Vertex 5
 VERTEX   40,   40,  -53,     6,     5,     9,     7,         31     \ Vertex 6
 VERTEX   40,  -40,  -53,     7,     1,     9,     8,         31     \ Vertex 7
 VERTEX   10,    0,  -53,     9,     9,     9,     9,         16     \ Vertex 8
 VERTEX    0,   -5,  -53,     9,     9,     9,     9,         16     \ Vertex 9
 VERTEX  -10,    0,  -53,     9,     9,     9,     9,          8     \ Vertex 10
 VERTEX    0,    5,  -53,     9,     9,     9,     9,          8     \ Vertex 11
 VERTEX    0,  -17,   71,    10,     0,    12,    11,         16     \ Vertex 12
 VERTEX    5,   -2,   61,    15,    15,     2,     0,          6     \ Vertex 13
 VERTEX    7,   23,   49,     1,     0,     4,    15,          7     \ Vertex 14
 VERTEX   21,    9,   49,     1,    10,    15,     3,          7     \ Vertex 15
 VERTEX   -5,   -2,   61,    11,     6,     3,     2,          6     \ Vertex 16
 VERTEX   -7,   23,   49,     8,    15,     0,    12,          7     \ Vertex 17
 VERTEX  -21,    9,   49,    15,     4,     8,     1,          7     \ Vertex 18

.SHIP_SHUTTLE_EDGES

    \ vertex1, vertex2, face1, face2, visibility
 EDGE       0,       1,     2,     0,         31    \ Edge 0
 EDGE       1,       2,    10,     4,         31    \ Edge 1
 EDGE       2,       3,    11,     6,         31    \ Edge 2
 EDGE       0,       3,    12,     8,         31    \ Edge 3
 EDGE       0,       7,     8,     1,         31    \ Edge 4
 EDGE       0,       4,     2,     1,         24    \ Edge 5
 EDGE       1,       4,     3,     2,         31    \ Edge 6
 EDGE       1,       5,     4,     3,         24    \ Edge 7
 EDGE       2,       5,     5,     4,         31    \ Edge 8
 EDGE       2,       6,     6,     5,         12    \ Edge 9
 EDGE       3,       6,     7,     6,         31    \ Edge 10
 EDGE       3,       7,     8,     7,         24    \ Edge 11
 EDGE       4,       5,     9,     3,         31    \ Edge 12
 EDGE       5,       6,     9,     5,         31    \ Edge 13
 EDGE       6,       7,     9,     7,         31    \ Edge 14
 EDGE       4,       7,     9,     1,         31    \ Edge 15
 EDGE       0,      12,    12,     0,         16    \ Edge 16
 EDGE       1,      12,    10,     0,         16    \ Edge 17
 EDGE       2,      12,    11,    10,         16    \ Edge 18
 EDGE       3,      12,    12,    11,         16    \ Edge 19
 EDGE       8,       9,     9,     9,         16    \ Edge 20
 EDGE       9,      10,     9,     9,          6    \ Edge 21
 EDGE      10,      11,     9,     9,          8    \ Edge 22
 EDGE       8,      11,     9,     9,          6    \ Edge 23
 EDGE      13,      14,    11,    11,          4    \ Edge 24
 EDGE      14,      15,    11,    11,          7    \ Edge 25
 EDGE      13,      15,    11,    11,          6    \ Edge 26
 EDGE      16,      17,    10,    10,          4    \ Edge 27
 EDGE      17,      18,    10,    10,          7    \ Edge 28
 EDGE      16,      18,    10,    10,          6    \ Edge 29

.SHIP_SHUTTLE_FACES

    \ normal_x, normal_y, normal_z, visibility
 FACE     -110,     -110,       80,         31    \ Face 0
 FACE        0,     -149,        7,         31    \ Face 1
 FACE     -102,     -102,       46,         31    \ Face 2
 FACE     -149,        0,        7,         31    \ Face 3
 FACE     -102,      102,       46,         31    \ Face 4
 FACE        0,      149,        7,         31    \ Face 5
 FACE      102,      102,       46,         31    \ Face 6
 FACE      149,        0,        7,         31    \ Face 7
 FACE      102,     -102,       46,         31    \ Face 8
 FACE        0,        0,     -213,         31    \ Face 9
 FACE      -81,       81,      177,         31    \ Face 10
 FACE       81,       81,      177,         31    \ Face 11
 FACE      110,     -110,       80,         31    \ Face 12

\ ******************************************************************************
\
\       Name: SHIP_TRANSPORTER
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprint for a Transporter
\  Deep dive: Ship blueprints
\
\ ******************************************************************************

.SHIP_TRANSPORTER

 EQUB 0                 \ Max. canisters on demise = 0
 EQUW 50 * 50           \ Targetable area          = 50 * 50

 EQUB LO(SHIP_TRANSPORTER_EDGES - SHIP_TRANSPORTER)   \ Edges data offset (low)
 EQUB LO(SHIP_TRANSPORTER_FACES - SHIP_TRANSPORTER)   \ Faces data offset (low)

 EQUB 145               \ Max. edge count          = (145 - 1) / 4 = 36
 EQUB 48                \ Gun vertex               = 48 / 4 = 12
 EQUB 26                \ Explosion count          = 5, as (4 * n) + 6 = 26
 EQUB 222               \ Number of vertices       = 222 / 6 = 37
 EQUB 46                \ Number of edges          = 46
 EQUW 0                 \ Bounty                   = 0
 EQUB 56                \ Number of faces          = 56 / 4 = 14
 EQUB 16                \ Visibility distance      = 16
 EQUB 32                \ Max. energy              = 32
 EQUB 10                \ Max. speed               = 10

 EQUB HI(SHIP_TRANSPORTER_EDGES - SHIP_TRANSPORTER)   \ Edges data offset (high)
 EQUB HI(SHIP_TRANSPORTER_FACES - SHIP_TRANSPORTER)   \ Faces data offset (high)

 EQUB 1                 \ Normals are scaled by    = 2^1 = 2
 EQUB %00000000         \ Laser power              = 0
                        \ Missiles                 = 0

.SHIP_TRANSPORTER_VERTICES

      \    x,    y,    z, face1, face2, face3, face4, visibility
 VERTEX    0,   19,  -51,     6,     0,     7,     7,         31     \ Vertex 0
 VERTEX  -51,    7,  -51,     1,     0,     7,     7,         31     \ Vertex 1
 VERTEX  -57,   -7,  -51,     1,     0,     2,     2,         31     \ Vertex 2
 VERTEX  -51,  -17,  -51,     2,     0,     3,     3,         31     \ Vertex 3
 VERTEX   51,  -17,  -51,     3,     0,     4,     4,         31     \ Vertex 4
 VERTEX   57,   -7,  -51,     4,     0,     5,     5,         31     \ Vertex 5
 VERTEX   51,    7,  -51,     5,     0,     6,     6,         31     \ Vertex 6
 VERTEX    0,   12,   24,    15,    15,    15,    15,         18     \ Vertex 7
 VERTEX  -60,   -2,   24,     7,     1,     9,     8,         31     \ Vertex 8
 VERTEX  -66,  -17,   24,     2,     1,     9,     3,         31     \ Vertex 9
 VERTEX   66,  -17,   24,     4,     3,    10,     5,         31     \ Vertex 10
 VERTEX   60,   -2,   24,     6,     5,    11,    10,         31     \ Vertex 11
 VERTEX  -22,   -5,   61,     9,     8,    13,    12,         31     \ Vertex 12
 VERTEX  -27,  -17,   61,     9,     3,    13,    13,         31     \ Vertex 13
 VERTEX   27,  -17,   61,    10,     3,    13,    13,         31     \ Vertex 14
 VERTEX   22,   -5,   61,    11,    10,    13,    12,         31     \ Vertex 15
 VERTEX  -10,   11,    5,     7,     7,     7,     7,          6     \ Vertex 16
 VERTEX  -36,    5,    5,     7,     7,     7,     7,          6     \ Vertex 17
 VERTEX  -10,   13,  -14,     7,     7,     7,     7,          6     \ Vertex 18
 VERTEX  -36,    7,  -14,     7,     7,     7,     7,          6     \ Vertex 19
 VERTEX  -23,   12,  -29,     7,     7,     7,     7,          6     \ Vertex 20
 VERTEX  -23,   10,  -14,     7,     7,     7,     7,          6     \ Vertex 21
 VERTEX   10,   15,  -29,     6,     6,     6,     6,          6     \ Vertex 22
 VERTEX   36,    9,  -29,     6,     6,     6,     6,          6     \ Vertex 23
 VERTEX   23,   10,  -14,     6,     6,     6,     6,          6     \ Vertex 24
 VERTEX   10,   12,   -6,     6,     6,     6,     6,          6     \ Vertex 25
 VERTEX   36,    6,   -6,     6,     6,     6,     6,          6     \ Vertex 26
 VERTEX   23,    7,   16,     6,     6,     6,     6,          6     \ Vertex 27
 VERTEX   23,    9,   -6,     6,     6,     6,     6,          6     \ Vertex 28
 VERTEX  -33,  -17,  -26,     3,     3,     3,     3,          5     \ Vertex 29
 VERTEX  -33,  -17,   33,     3,     3,     3,     3,          5     \ Vertex 30
 VERTEX   33,  -17,  -26,     3,     3,     3,     3,          5     \ Vertex 31
 VERTEX   33,  -17,   33,     3,     3,     3,     3,          5     \ Vertex 32
 VERTEX  -25,   -6,  -51,     0,     0,     0,     0,          7     \ Vertex 33
 VERTEX   26,   -6,  -51,     0,     0,     0,     0,          7     \ Vertex 34
 VERTEX   17,    6,  -51,     0,     0,     0,     0,          4     \ Vertex 35
 VERTEX  -17,    6,  -51,     0,     0,     0,     0,          4     \ Vertex 36

.SHIP_TRANSPORTER_EDGES

    \ vertex1, vertex2, face1, face2, visibility
 EDGE       0,       1,     7,     0,         31    \ Edge 0
 EDGE       1,       2,     1,     0,         31    \ Edge 1
 EDGE       2,       3,     2,     0,         31    \ Edge 2
 EDGE       3,       4,     3,     0,         31    \ Edge 3
 EDGE       4,       5,     4,     0,         31    \ Edge 4
 EDGE       5,       6,     5,     0,         31    \ Edge 5
 EDGE       0,       6,     6,     0,         31    \ Edge 6
 EDGE       0,       7,     7,     6,         15    \ Edge 7
 EDGE       1,       8,     7,     1,         31    \ Edge 8
 EDGE       2,       9,     2,     1,         10    \ Edge 9
 EDGE       3,       9,     3,     2,         31    \ Edge 10
 EDGE       4,      10,     4,     3,         31    \ Edge 11
 EDGE       5,      10,     5,     4,         10    \ Edge 12
 EDGE       6,      11,     6,     5,         31    \ Edge 13
 EDGE       7,       8,     8,     7,         16    \ Edge 14
 EDGE       8,       9,     9,     1,         16    \ Edge 15
 EDGE      10,      11,    10,     5,         16    \ Edge 16
 EDGE       7,      11,    11,     6,         16    \ Edge 17
 EDGE       7,      15,    12,    11,         18    \ Edge 18
 EDGE       7,      12,    12,     8,         18    \ Edge 19
 EDGE       8,      12,     9,     8,         16    \ Edge 20
 EDGE       9,      13,     9,     3,         31    \ Edge 21
 EDGE      10,      14,    10,     3,         31    \ Edge 22
 EDGE      11,      15,    11,    10,         16    \ Edge 23
 EDGE      12,      13,    13,     9,         31    \ Edge 24
 EDGE      13,      14,    13,     3,         31    \ Edge 25
 EDGE      14,      15,    13,    10,         31    \ Edge 26
 EDGE      12,      15,    13,    12,         31    \ Edge 27
 EDGE      16,      17,     7,     7,          6    \ Edge 28
 EDGE      18,      19,     7,     7,          6    \ Edge 29
 EDGE      19,      20,     7,     7,          6    \ Edge 30
 EDGE      18,      20,     7,     7,          6    \ Edge 31
 EDGE      20,      21,     7,     7,          6    \ Edge 32
 EDGE      22,      23,     6,     6,          6    \ Edge 33
 EDGE      23,      24,     6,     6,          6    \ Edge 34
 EDGE      24,      22,     6,     6,          6    \ Edge 35
 EDGE      25,      26,     6,     6,          6    \ Edge 36
 EDGE      26,      27,     6,     6,          6    \ Edge 37
 EDGE      25,      27,     6,     6,          6    \ Edge 38
 EDGE      27,      28,     6,     6,          6    \ Edge 39
 EDGE      29,      30,     3,     3,          5    \ Edge 40
 EDGE      31,      32,     3,     3,          5    \ Edge 41
 EDGE      33,      34,     0,     0,          7    \ Edge 42
 EDGE      34,      35,     0,     0,          4    \ Edge 43
 EDGE      35,      36,     0,     0,          4    \ Edge 44
 EDGE      36,      33,     0,     0,          4    \ Edge 45

.SHIP_TRANSPORTER_FACES

    \ normal_x, normal_y, normal_z, visibility
 FACE        0,        0,     -103,         31    \ Face 0
 FACE     -111,       48,       -7,         31    \ Face 1
 FACE     -105,      -63,      -21,         31    \ Face 2
 FACE        0,      -34,        0,         31    \ Face 3
 FACE      105,      -63,      -21,         31    \ Face 4
 FACE      111,       48,       -7,         31    \ Face 5
 FACE        8,       32,        3,         31    \ Face 6
 FACE       -8,       32,        3,         31    \ Face 7
 FACE       -8,       34,       11,         18    \ Face 8
 FACE      -75,       32,       79,         31    \ Face 9
 FACE       75,       32,       79,         31    \ Face 10
 FACE        8,       34,       11,         18    \ Face 11
 FACE        0,       38,       17,         31    \ Face 12
 FACE        0,        0,      121,         31    \ Face 13

\ ******************************************************************************
\
\       Name: SHIP_COBRA_MK_3
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprint for a Cobra Mk III
\  Deep dive: Ship blueprints
\
\ ******************************************************************************

.SHIP_COBRA_MK_3

 EQUB 3                 \ Max. canisters on demise = 3
 EQUW 95 * 95           \ Targetable area          = 95 * 95

 EQUB LO(SHIP_COBRA_MK_3_EDGES - SHIP_COBRA_MK_3)  \ Edges data offset (low)
 EQUB LO(SHIP_COBRA_MK_3_FACES - SHIP_COBRA_MK_3)  \ Faces data offset (low)

 EQUB 153               \ Max. edge count          = (153 - 1) / 4 = 38
 EQUB 84                \ Gun vertex               = 84 / 4 = 21
 EQUB 42                \ Explosion count          = 9, as (4 * n) + 6 = 42
 EQUB 168               \ Number of vertices       = 168 / 6 = 28
 EQUB 38                \ Number of edges          = 38
 EQUW 0                 \ Bounty                   = 0
 EQUB 52                \ Number of faces          = 52 / 4 = 13
 EQUB 50                \ Visibility distance      = 50
 EQUB 150               \ Max. energy              = 150
 EQUB 28                \ Max. speed               = 28

 EQUB HI(SHIP_COBRA_MK_3_EDGES - SHIP_COBRA_MK_3)  \ Edges data offset (low)
 EQUB HI(SHIP_COBRA_MK_3_FACES - SHIP_COBRA_MK_3)  \ Faces data offset (low)

 EQUB 1                 \ Normals are scaled by    = 2^1 = 2
 EQUB %00010011         \ Laser power              = 2
                        \ Missiles                 = 3

.SHIP_COBRA_MK_3_VERTICES

      \    x,    y,    z, face1, face2, face3, face4, visibility
 VERTEX   32,    0,   76,    15,     15,   15,    15,         31    \ Vertex 0
 VERTEX  -32,    0,   76,    15,     15,   15,    15,         31    \ Vertex 1
 VERTEX    0,   26,   24,    15,     15,   15,    15,         31    \ Vertex 2
 VERTEX -120,   -3,   -8,     3,      7,   10,    10,         31    \ Vertex 3
 VERTEX  120,   -3,   -8,     4,      8,   12,    12,         31    \ Vertex 4
 VERTEX  -88,   16,  -40,    15,     15,   15,    15,         31    \ Vertex 5
 VERTEX   88,   16,  -40,    15,     15,   15,    15,         31    \ Vertex 6
 VERTEX  128,   -8,  -40,     8,      9,   12,    12,         31    \ Vertex 7
 VERTEX -128,   -8,  -40,     7,      9,   10,    10,         31    \ Vertex 8
 VERTEX    0,   26,  -40,     5,      6,    9,     9,         31    \ Vertex 9
 VERTEX  -32,  -24,  -40,     9,     10,   11,    11,         31    \ Vertex 10
 VERTEX   32,  -24,  -40,     9,     11,   12,    12,         31    \ Vertex 11
 VERTEX  -36,    8,  -40,     9,      9,    9,     9,         20    \ Vertex 12
 VERTEX   -8,   12,  -40,     9,      9,    9,     9,         20    \ Vertex 13
 VERTEX    8,   12,  -40,     9,      9,    9,     9,         20    \ Vertex 14
 VERTEX   36,    8,  -40,     9,      9,    9,     9,         20    \ Vertex 15
 VERTEX   36,  -12,  -40,     9,      9,    9,     9,         20    \ Vertex 16
 VERTEX    8,  -16,  -40,     9,      9,    9,     9,         20    \ Vertex 17
 VERTEX   -8,  -16,  -40,     9,      9,    9,     9,         20    \ Vertex 18
 VERTEX  -36,  -12,  -40,     9,      9,    9,     9,         20    \ Vertex 19
 VERTEX    0,    0,   76,     0,     11,   11,    11,          6    \ Vertex 20
 VERTEX    0,    0,   90,     0,     11,   11,    11,         31    \ Vertex 21
 VERTEX  -80,   -6,  -40,     9,      9,    9,     9,          8    \ Vertex 22
 VERTEX  -80,    6,  -40,     9,      9,    9,     9,          8    \ Vertex 23
 VERTEX  -88,    0,  -40,     9,      9,    9,     9,          6    \ Vertex 24
 VERTEX   80,    6,  -40,     9,      9,    9,     9,          8    \ Vertex 25
 VERTEX   88,    0,  -40,     9,      9,    9,     9,          6    \ Vertex 26
 VERTEX   80,   -6,  -40,     9,      9,    9,     9,          8    \ Vertex 27

.SHIP_COBRA_MK_3_EDGES

    \ vertex1, vertex2, face1, face2, visibility
 EDGE       0,       1,     0,    11,         31    \ Edge 0
 EDGE       0,       4,     4,    12,         31    \ Edge 1
 EDGE       1,       3,     3,    10,         31    \ Edge 2
 EDGE       3,       8,     7,    10,         31    \ Edge 3
 EDGE       4,       7,     8,    12,         31    \ Edge 4
 EDGE       6,       7,     8,     9,         31    \ Edge 5
 EDGE       6,       9,     6,     9,         31    \ Edge 6
 EDGE       5,       9,     5,     9,         31    \ Edge 7
 EDGE       5,       8,     7,     9,         31    \ Edge 8
 EDGE       2,       5,     1,     5,         31    \ Edge 9
 EDGE       2,       6,     2,     6,         31    \ Edge 10
 EDGE       3,       5,     3,     7,         31    \ Edge 11
 EDGE       4,       6,     4,     8,         31    \ Edge 12
 EDGE       1,       2,     0,     1,         31    \ Edge 13
 EDGE       0,       2,     0,     2,         31    \ Edge 14
 EDGE       8,      10,     9,    10,         31    \ Edge 15
 EDGE      10,      11,     9,    11,         31    \ Edge 16
 EDGE       7,      11,     9,    12,         31    \ Edge 17
 EDGE       1,      10,    10,    11,         31    \ Edge 18
 EDGE       0,      11,    11,    12,         31    \ Edge 19
 EDGE       1,       5,     1,     3,         29    \ Edge 20
 EDGE       0,       6,     2,     4,         29    \ Edge 21
 EDGE      20,      21,     0,    11,          6    \ Edge 22
 EDGE      12,      13,     9,     9,         20    \ Edge 23
 EDGE      18,      19,     9,     9,         20    \ Edge 24
 EDGE      14,      15,     9,     9,         20    \ Edge 25
 EDGE      16,      17,     9,     9,         20    \ Edge 26
 EDGE      15,      16,     9,     9,         19    \ Edge 27
 EDGE      14,      17,     9,     9,         17    \ Edge 28
 EDGE      13,      18,     9,     9,         19    \ Edge 29
 EDGE      12,      19,     9,     9,         19    \ Edge 30
 EDGE       2,       9,     5,     6,         30    \ Edge 31
 EDGE      22,      24,     9,     9,          6    \ Edge 32
 EDGE      23,      24,     9,     9,          6    \ Edge 33
 EDGE      22,      23,     9,     9,          8    \ Edge 34
 EDGE      25,      26,     9,     9,          6    \ Edge 35
 EDGE      26,      27,     9,     9,          6    \ Edge 36
 EDGE      25,      27,     9,     9,          8    \ Edge 37

.SHIP_COBRA_MK_3_FACES

    \ normal_x, normal_y, normal_z, visibility
 FACE        0,       62,       31,         31    \ Face 0
 FACE      -18,       55,       16,         31    \ Face 1
 FACE       18,       55,       16,         31    \ Face 2
 FACE      -16,       52,       14,         31    \ Face 3
 FACE       16,       52,       14,         31    \ Face 4
 FACE      -14,       47,        0,         31    \ Face 5
 FACE       14,       47,        0,         31    \ Face 6
 FACE      -61,      102,        0,         31    \ Face 7
 FACE       61,      102,        0,         31    \ Face 8
 FACE        0,        0,      -80,         31    \ Face 9
 FACE       -7,      -42,        9,         31    \ Face 10
 FACE        0,      -30,        6,         31    \ Face 11
 FACE        7,      -42,        9,         31    \ Face 12

\ ******************************************************************************
\
\       Name: SHIP_PYTHON
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprint for a Python
\  Deep dive: Ship blueprints
\
\ ******************************************************************************

.SHIP_PYTHON

 EQUB 5                 \ Max. canisters on demise = 5
 EQUW 80 * 80           \ Targetable area          = 80 * 80

 EQUB LO(SHIP_PYTHON_EDGES - SHIP_PYTHON)          \ Edges data offset (low)
 EQUB LO(SHIP_PYTHON_FACES - SHIP_PYTHON)          \ Faces data offset (low)

 EQUB 85                \ Max. edge count          = (85 - 1) / 4 = 21
 EQUB 0                 \ Gun vertex               = 0
 EQUB 42                \ Explosion count          = 9, as (4 * n) + 6 = 42
 EQUB 66                \ Number of vertices       = 66 / 6 = 11
 EQUB 26                \ Number of edges          = 26
 EQUW 0                 \ Bounty                   = 0
 EQUB 52                \ Number of faces          = 52 / 4 = 13
 EQUB 40                \ Visibility distance      = 40
 EQUB 250               \ Max. energy              = 250
 EQUB 20                \ Max. speed               = 20

 EQUB HI(SHIP_PYTHON_EDGES - SHIP_PYTHON)          \ Edges data offset (high)
 EQUB HI(SHIP_PYTHON_FACES - SHIP_PYTHON)          \ Faces data offset (high)

 EQUB 0                 \ Normals are scaled by    = 2^0 = 1
 EQUB %00011011         \ Laser power              = 3
                        \ Missiles                 = 3

.SHIP_PYTHON_VERTICES

      \    x,    y,    z, face1, face2, face3, face4, visibility
 VERTEX    0,    0,  224,     0,      1,    2,     3,         31    \ Vertex 0
 VERTEX    0,   48,   48,     0,      1,    4,     5,         30    \ Vertex 1
 VERTEX   96,    0,  -16,    15,     15,   15,    15,         31    \ Vertex 2
 VERTEX  -96,    0,  -16,    15,     15,   15,    15,         31    \ Vertex 3
 VERTEX    0,   48,  -32,     4,      5,    8,     9,         30    \ Vertex 4
 VERTEX    0,   24, -112,     9,      8,   12,    12,         31    \ Vertex 5
 VERTEX  -48,    0, -112,     8,     11,   12,    12,         31    \ Vertex 6
 VERTEX   48,    0, -112,     9,     10,   12,    12,         31    \ Vertex 7
 VERTEX    0,  -48,   48,     2,      3,    6,     7,         30    \ Vertex 8
 VERTEX    0,  -48,  -32,     6,      7,   10,    11,         30    \ Vertex 9
 VERTEX    0,  -24, -112,    10,     11,   12,    12,         30    \ Vertex 10

.SHIP_PYTHON_EDGES

    \ vertex1, vertex2, face1, face2, visibility
 EDGE       0,       8,     2,     3,         30    \ Edge 0
 EDGE       0,       3,     0,     2,         31    \ Edge 1
 EDGE       0,       2,     1,     3,         31    \ Edge 2
 EDGE       0,       1,     0,     1,         30    \ Edge 3
 EDGE       2,       4,     9,     5,         29    \ Edge 4
 EDGE       1,       2,     1,     5,         29    \ Edge 5
 EDGE       2,       8,     7,     3,         29    \ Edge 6
 EDGE       1,       3,     0,     4,         29    \ Edge 7
 EDGE       3,       8,     2,     6,         29    \ Edge 8
 EDGE       2,       9,     7,    10,         29    \ Edge 9
 EDGE       3,       4,     4,     8,         29    \ Edge 10
 EDGE       3,       9,     6,    11,         29    \ Edge 11
 EDGE       3,       5,     8,     8,          5    \ Edge 12
 EDGE       3,      10,    11,    11,          5    \ Edge 13
 EDGE       2,       5,     9,     9,          5    \ Edge 14
 EDGE       2,      10,    10,    10,          5    \ Edge 15
 EDGE       2,       7,     9,    10,         31    \ Edge 16
 EDGE       3,       6,     8,    11,         31    \ Edge 17
 EDGE       5,       6,     8,    12,         31    \ Edge 18
 EDGE       5,       7,     9,    12,         31    \ Edge 19
 EDGE       7,      10,    12,    10,         29    \ Edge 20
 EDGE       6,      10,    11,    12,         29    \ Edge 21
 EDGE       4,       5,     8,     9,         29    \ Edge 22
 EDGE       9,      10,    10,    11,         29    \ Edge 23
 EDGE       1,       4,     4,     5,         29    \ Edge 24
 EDGE       8,       9,     6,     7,         29    \ Edge 25

.SHIP_PYTHON_FACES

    \ normal_x, normal_y, normal_z, visibility
 FACE      -27,       40,       11,         30    \ Face 0
 FACE       27,       40,       11,         30    \ Face 1
 FACE      -27,      -40,       11,         30    \ Face 2
 FACE       27,      -40,       11,         30    \ Face 3
 FACE      -19,       38,        0,         30    \ Face 4
 FACE       19,       38,        0,         30    \ Face 5
 FACE      -19,      -38,        0,         30    \ Face 6
 FACE       19,      -38,        0,         30    \ Face 7
 FACE      -25,       37,      -11,         30    \ Face 8
 FACE       25,       37,      -11,         30    \ Face 9
 FACE       25,      -37,      -11,         30    \ Face 10
 FACE      -25,      -37,      -11,         30    \ Face 11
 FACE        0,        0,     -112,         30    \ Face 12

\ ******************************************************************************
\
\       Name: SHIP_VIPER
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprint for a Viper
\  Deep dive: Ship blueprints
\
\ ******************************************************************************

.SHIP_VIPER

 EQUB 0                 \ Max. canisters on demise = 0
 EQUW 75 * 75           \ Targetable area          = 75 * 75

 EQUB LO(SHIP_VIPER_EDGES - SHIP_VIPER)            \ Edges data offset (low)
 EQUB LO(SHIP_VIPER_FACES - SHIP_VIPER)            \ Faces data offset (low)

 EQUB 77                \ Max. edge count          = (77 - 1) / 4 = 19
 EQUB 0                 \ Gun vertex               = 0
 EQUB 42                \ Explosion count          = 9, as (4 * n) + 6 = 42
 EQUB 90                \ Number of vertices       = 90 / 6 = 15
 EQUB 20                \ Number of edges          = 20
 EQUW 0                 \ Bounty                   = 0
 EQUB 28                \ Number of faces          = 28 / 4 = 7
 EQUB 23                \ Visibility distance      = 23
 EQUB 100               \ Max. energy              = 100
 EQUB 32                \ Max. speed               = 32

 EQUB HI(SHIP_VIPER_EDGES - SHIP_VIPER)            \ Edges data offset (high)
 EQUB HI(SHIP_VIPER_FACES - SHIP_VIPER)            \ Faces data offset (high)

 EQUB 1                 \ Normals are scaled by    = 2^1 = 2
 EQUB %00010001         \ Laser power              = 2
                        \ Missiles                 = 1

.SHIP_VIPER_VERTICES

      \    x,    y,    z, face1, face2, face3, face4, visibility
 VERTEX    0,    0,   72,     1,      2,    3,     4,         31    \ Vertex 0
 VERTEX    0,   16,   24,     0,      1,    2,     2,         30    \ Vertex 1
 VERTEX    0,  -16,   24,     3,      4,    5,     5,         30    \ Vertex 2
 VERTEX   48,    0,  -24,     2,      4,    6,     6,         31    \ Vertex 3
 VERTEX  -48,    0,  -24,     1,      3,    6,     6,         31    \ Vertex 4
 VERTEX   24,  -16,  -24,     4,      5,    6,     6,         30    \ Vertex 5
 VERTEX  -24,  -16,  -24,     5,      3,    6,     6,         30    \ Vertex 6
 VERTEX   24,   16,  -24,     0,      2,    6,     6,         31    \ Vertex 7
 VERTEX  -24,   16,  -24,     0,      1,    6,     6,         31    \ Vertex 8
 VERTEX  -32,    0,  -24,     6,      6,    6,     6,         19    \ Vertex 9
 VERTEX   32,    0,  -24,     6,      6,    6,     6,         19    \ Vertex 10
 VERTEX    8,    8,  -24,     6,      6,    6,     6,         19    \ Vertex 11
 VERTEX   -8,    8,  -24,     6,      6,    6,     6,         19    \ Vertex 12
 VERTEX   -8,   -8,  -24,     6,      6,    6,     6,         18    \ Vertex 13
 VERTEX    8,   -8,  -24,     6,      6,    6,     6,         18    \ Vertex 14

.SHIP_VIPER_EDGES

    \ vertex1, vertex2, face1, face2, visibility
 EDGE       0,       3,     2,     4,         31    \ Edge 0
 EDGE       0,       1,     1,     2,         30    \ Edge 1
 EDGE       0,       2,     3,     4,         30    \ Edge 2
 EDGE       0,       4,     1,     3,         31    \ Edge 3
 EDGE       1,       7,     0,     2,         30    \ Edge 4
 EDGE       1,       8,     0,     1,         30    \ Edge 5
 EDGE       2,       5,     4,     5,         30    \ Edge 6
 EDGE       2,       6,     3,     5,         30    \ Edge 7
 EDGE       7,       8,     0,     6,         31    \ Edge 8
 EDGE       5,       6,     5,     6,         30    \ Edge 9
 EDGE       4,       8,     1,     6,         31    \ Edge 10
 EDGE       4,       6,     3,     6,         30    \ Edge 11
 EDGE       3,       7,     2,     6,         31    \ Edge 12
 EDGE       3,       5,     6,     4,         30    \ Edge 13
 EDGE       9,      12,     6,     6,         19    \ Edge 14
 EDGE       9,      13,     6,     6,         18    \ Edge 15
 EDGE      10,      11,     6,     6,         19    \ Edge 16
 EDGE      10,      14,     6,     6,         18    \ Edge 17
 EDGE      11,      14,     6,     6,         16    \ Edge 18
 EDGE      12,      13,     6,     6,         16    \ Edge 19

.SHIP_VIPER_FACES

    \ normal_x, normal_y, normal_z, visibility
 FACE        0,       32,        0,         31    \ Face 0
 FACE      -22,       33,       11,         31    \ Face 1
 FACE       22,       33,       11,         31    \ Face 2
 FACE      -22,      -33,       11,         31    \ Face 3
 FACE       22,      -33,       11,         31    \ Face 4
 FACE        0,      -32,        0,         31    \ Face 5
 FACE        0,        0,      -48,         31    \ Face 6

\ ******************************************************************************
\
\       Name: SHIP_KRAIT
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprint for a Krait
\  Deep dive: Ship blueprints
\
\ ******************************************************************************

.SHIP_KRAIT

 EQUB 1                 \ Max. canisters on demise = 1
 EQUW 60 * 60           \ Targetable area          = 60 * 60

 EQUB LO(SHIP_KRAIT_EDGES - SHIP_KRAIT)            \ Edges data offset (low)
 EQUB LO(SHIP_KRAIT_FACES - SHIP_KRAIT)            \ Faces data offset (low)

 EQUB 85                \ Max. edge count          = (85 - 1) / 4 = 21
 EQUB 0                 \ Gun vertex               = 0
 EQUB 18                \ Explosion count          = 3, as (4 * n) + 6 = 18
 EQUB 102               \ Number of vertices       = 102 / 6 = 17
 EQUB 21                \ Number of edges          = 21
 EQUW 100               \ Bounty                   = 100
 EQUB 24                \ Number of faces          = 24 / 4 = 6
 EQUB 20                \ Visibility distance      = 20
 EQUB 80                \ Max. energy              = 80
 EQUB 30                \ Max. speed               = 30

 EQUB HI(SHIP_KRAIT_EDGES - SHIP_KRAIT)            \ Edges data offset (high)
 EQUB HI(SHIP_KRAIT_FACES - SHIP_KRAIT)            \ Faces data offset (high)

 EQUB 2                 \ Normals are scaled by    = 2^2 = 4
 EQUB %00010000         \ Laser power              = 2
                        \ Missiles                 = 0

.SHIP_KRAIT_VERTICES

      \    x,    y,    z, face1, face2, face3, face4, visibility
 VERTEX    0,    0,   96,     1,      0,    3,     2,         31    \ Vertex 0
 VERTEX    0,   18,  -48,     3,      0,    5,     4,         31    \ Vertex 1
 VERTEX    0,  -18,  -48,     2,      1,    5,     4,         31    \ Vertex 2
 VERTEX   90,    0,   -3,     1,      0,    4,     4,         31    \ Vertex 3
 VERTEX  -90,    0,   -3,     3,      2,    5,     5,         31    \ Vertex 4
 VERTEX   90,    0,   87,     1,     0,     1,     1,         28     \ Vertex 5
 VERTEX  -90,    0,   87,     3,     2,     3,     3,         28     \ Vertex 6
 VERTEX    0,    5,   53,     0,      0,    3,     3,          9    \ Vertex 7
 VERTEX    0,    7,   38,     0,      0,    3,     3,          6    \ Vertex 8
 VERTEX  -18,    7,   19,     3,      3,    3,     3,          9    \ Vertex 9
 VERTEX   18,    7,   19,     0,      0,    0,     0,          9    \ Vertex 10
 VERTEX   18,   11,  -39,     4,      4,    4,     4,          8    \ Vertex 11
 VERTEX   18,  -11,  -39,     4,      4,    4,     4,          8    \ Vertex 12
 VERTEX   36,    0,  -30,     4,      4,    4,     4,          8    \ Vertex 13
 VERTEX  -18,   11,  -39,     5,      5,    5,     5,          8    \ Vertex 14
 VERTEX  -18,  -11,  -39,     5,      5,    5,     5,          8    \ Vertex 15
 VERTEX  -36,    0,  -30,     5,      5,    5,     5,          8    \ Vertex 16

.SHIP_KRAIT_EDGES

    \ vertex1, vertex2, face1, face2, visibility
 EDGE       0,       1,     3,     0,         31    \ Edge 0
 EDGE       0,       2,     2,     1,         31    \ Edge 1
 EDGE       0,       3,     1,     0,         31    \ Edge 2
 EDGE       0,       4,     3,     2,         31    \ Edge 3
 EDGE       1,       4,     5,     3,         31    \ Edge 4
 EDGE       4,       2,     5,     2,         31    \ Edge 5
 EDGE       2,       3,     4,     1,         31    \ Edge 6
 EDGE       3,       1,     4,     0,         31    \ Edge 7
 EDGE       3,       5,     1,     0,         28    \ Edge 8
 EDGE       4,       6,     3,     2,         28    \ Edge 9
 EDGE       1,       2,     5,     4,          5    \ Edge 10
 EDGE       7,      10,     0,     0,          9    \ Edge 11
 EDGE       8,      10,     0,     0,          6    \ Edge 12
 EDGE       7,       9,     3,     3,          9    \ Edge 13
 EDGE       8,       9,     3,     3,          6    \ Edge 14
 EDGE      11,      13,     4,     4,          8    \ Edge 15
 EDGE      13,      12,     4,     4,          8    \ Edge 16
 EDGE      12,      11,     4,     4,          7    \ Edge 17
 EDGE      14,      15,     5,     5,          7    \ Edge 18
 EDGE      15,      16,     5,     5,          8    \ Edge 19
 EDGE      16,      14,     5,     5,          8    \ Edge 20

.SHIP_KRAIT_FACES

    \ normal_x, normal_y, normal_z, visibility
 FACE        7,       48,        6,         31    \ Face 0
 FACE        7,      -48,        6,         31    \ Face 1
 FACE       -7,      -48,        6,         31    \ Face 2
 FACE       -7,       48,        6,         31    \ Face 3
 FACE       77,        0,     -154,         31    \ Face 4
 FACE      -77,        0,     -154,         31    \ Face 5

\ ******************************************************************************
\
\       Name: SHIP_CONSTRICTOR
\       Type: Variable
\   Category: Drawing ships
\    Summary: Ship blueprint for a Constrictor
\  Deep dive: Ship blueprints
\
\ ******************************************************************************

.SHIP_CONSTRICTOR

 EQUB 3 + (15 << 4)     \ Max. canisters on demise = 3
                        \ Market item when scooped = 15 + 1 = 16 (alien items)
 EQUW 99 * 99           \ Targetable area          = 99 * 99

 EQUB LO(SHIP_CONSTRICTOR_EDGES - SHIP_CONSTRICTOR)   \ Edges data offset (low)
 EQUB LO(SHIP_CONSTRICTOR_FACES - SHIP_CONSTRICTOR)   \ Faces data offset (low)

 EQUB 77                \ Max. edge count          = (77 - 1) / 4 = 19
 EQUB 0                 \ Gun vertex               = 0
 EQUB 46                \ Explosion count          = 10, as (4 * n) + 6 = 46
 EQUB 102               \ Number of vertices       = 102 / 6 = 17
 EQUB 24                \ Number of edges          = 24
 EQUW 0                 \ Bounty                   = 0
 EQUB 40                \ Number of faces          = 40 / 4 = 10
 EQUB 45                \ Visibility distance      = 45
 EQUB 200               \ Max. energy              = 200
 EQUB 55                \ Max. speed               = 55

 EQUB HI(SHIP_CONSTRICTOR_EDGES - SHIP_CONSTRICTOR)   \ Edges data offset (high)
 EQUB HI(SHIP_CONSTRICTOR_FACES - SHIP_CONSTRICTOR)   \ Faces data offset (high)

 EQUB 2                 \ Normals are scaled by    = 2^2 = 4
 EQUB %00101111         \ Laser power              = 5
                        \ Missiles                 = 7

.SHIP_CONSTRICTOR_VERTICES

      \    x,    y,    z, face1, face2, face3, face4, visibility
 VERTEX   20,   -7,   80,     2,      0,    9,     9,         31    \ Vertex 0
 VERTEX  -20,   -7,   80,     1,      0,    9,     9,         31    \ Vertex 1
 VERTEX  -54,   -7,   40,     4,      1,    9,     9,         31    \ Vertex 2
 VERTEX  -54,   -7,  -40,     5,      4,    9,     8,         31    \ Vertex 3
 VERTEX  -20,   13,  -40,     6,      5,    8,     8,         31    \ Vertex 4
 VERTEX   20,   13,  -40,     7,      6,    8,     8,         31    \ Vertex 5
 VERTEX   54,   -7,  -40,     7,      3,    9,     8,         31    \ Vertex 6
 VERTEX   54,   -7,   40,     3,      2,    9,     9,         31    \ Vertex 7
 VERTEX   20,   13,    5,    15,     15,   15,    15,         31    \ Vertex 8
 VERTEX  -20,   13,    5,    15,     15,   15,    15,         31    \ Vertex 9
 VERTEX   20,   -7,   62,     9,      9,    9,     9,         18    \ Vertex 10
 VERTEX  -20,   -7,   62,     9,      9,    9,     9,         18    \ Vertex 11
 VERTEX   25,   -7,  -25,     9,      9,    9,     9,         18    \ Vertex 12
 VERTEX  -25,   -7,  -25,     9,      9,    9,     9,         18    \ Vertex 13
 VERTEX   15,   -7,  -15,     9,      9,    9,     9,         10    \ Vertex 14
 VERTEX  -15,   -7,  -15,     9,      9,    9,     9,         10    \ Vertex 15
 VERTEX    0,   -7,    0,    15,      9,    1,     0,          0    \ Vertex 16

.SHIP_CONSTRICTOR_EDGES

    \ vertex1, vertex2, face1, face2, visibility
 EDGE       0,       1,     9,     0,         31    \ Edge 0
 EDGE       1,       2,     9,     1,         31    \ Edge 1
 EDGE       1,       9,     1,     0,         31    \ Edge 2
 EDGE       0,       8,     2,     0,         31    \ Edge 3
 EDGE       0,       7,     9,     2,         31    \ Edge 4
 EDGE       7,       8,     3,     2,         31    \ Edge 5
 EDGE       2,       9,     4,     1,         31    \ Edge 6
 EDGE       2,       3,     9,     4,         31    \ Edge 7
 EDGE       6,       7,     9,     3,         31    \ Edge 8
 EDGE       6,       8,     7,     3,         31    \ Edge 9
 EDGE       5,       8,     7,     6,         31    \ Edge 10
 EDGE       4,       9,     6,     5,         31    \ Edge 11
 EDGE       3,       9,     5,     4,         31    \ Edge 12
 EDGE       3,       4,     8,     5,         31    \ Edge 13
 EDGE       4,       5,     8,     6,         31    \ Edge 14
 EDGE       5,       6,     8,     7,         31    \ Edge 15
 EDGE       3,       6,     9,     8,         31    \ Edge 16
 EDGE       8,       9,     6,     0,         31    \ Edge 17
 EDGE      10,      12,     9,     9,         18    \ Edge 18
 EDGE      12,      14,     9,     9,          5    \ Edge 19
 EDGE      14,      10,     9,     9,         10    \ Edge 20
 EDGE      11,      15,     9,     9,         10    \ Edge 21
 EDGE      13,      15,     9,     9,          5    \ Edge 22
 EDGE      11,      13,     9,     9,         18    \ Edge 23

.SHIP_CONSTRICTOR_FACES

    \ normal_x, normal_y, normal_z, visibility
 FACE        0,       55,       15,         31    \ Face 0
 FACE      -24,       75,       20,         31    \ Face 1
 FACE       24,       75,       20,         31    \ Face 2
 FACE       44,       75,        0,         31    \ Face 3
 FACE      -44,       75,        0,         31    \ Face 4
 FACE      -44,       75,        0,         31    \ Face 5
 FACE        0,       53,        0,         31    \ Face 6
 FACE       44,       75,        0,         31    \ Face 7
 FACE        0,        0,     -160,         31    \ Face 8
 FACE        0,      -27,        0,         31    \ Face 9

 MODE7_HIGH_Y = 3*16    \ The last sixel y-coordinate we can draw sixels in + 1
                        \ (so row 16 is safe)

 INCLUDE "1-source-files/main-sources/elite-teletext-sixels.asm"

 INCLUDE "1-source-files/main-sources/elite-teletext-lines.asm"

 INCLUDE "1-source-files/main-sources/elite-teletext-text.asm"

\ ******************************************************************************
\
\ Save MENU2.bin
\
\ ******************************************************************************

 PRINT "S.MENU2 ", ~CODE%, " ", ~P%, " ", ~LOAD%, " ", ~LOAD%
 SAVE "3-assembled-output/MENU2.bin", CODE%, P%

