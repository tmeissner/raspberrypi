-- raspilcd, a simple tool to display bmp pictures & text on a ST7565 LCD
-- Copyright (C) 2014  Torsten Meissner
-- 
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- 
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
-- 
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see http://www.gnu.org/licenses/.



with Interfaces;
with Interfaces.C;


package st7565lcd is


    -- type definitions
    subtype byte  is Interfaces.Unsigned_8;
    subtype word  is Interfaces.Unsigned_16;
    subtype dword is Interfaces.Unsigned_32;

    type t_byte_array is array (natural range <>) of byte;
    
    type t_font_array is array (natural range <>) of t_byte_array (0 .. 4);

    type t_bmp_array is array (natural range <>) of t_byte_array (0 .. 3);

    subtype t_lcd_array is t_byte_array (0 .. 128 * 8 - 1);


    -- bmp header structure
    type t_bmp_header is record
        bfType          : word;
        bfSize          : dword;
        bfReserved      : dword;
        bfOffBits       : dword;
        biSize          : dword;
        biWidth         : integer;
        biHeight        : integer;
        biPlanes        : word;
        biBitCount      : word;
        biCompression   : dword;
        biSizeImage     : dword;
        biXPelsPerMeter : integer;
        biYPelsPerMeter : integer;
        biClrUsed       : dword;
        biClrImportant  : dword;
    end record;

    -- color mask
    type t_color_mask is record
        red   : dword := 16#00FF0000#;
        green : dword := 16#0000FF00#;
        blue  : dword := 16#000000FF#;
        alpha : dword := 16#FF000000#;
    end record;


    -- character set
    font_5x7 : constant t_font_array (32 .. 126) := (
        ( 16#00#, 16#00#, 16#00#, 16#00#, 16#00# ),  --   - 16#20 - 32
        ( 16#00#, 16#00#, 16#5f#, 16#00#, 16#00# ),  -- ! - 16#21 - 33
        ( 16#00#, 16#07#, 16#00#, 16#07#, 16#00# ),  -- " - 16#22 - 34
        ( 16#14#, 16#7f#, 16#14#, 16#7f#, 16#14# ),  -- # - 16#23 - 35
        ( 16#24#, 16#2a#, 16#7f#, 16#2a#, 16#12# ),  -- $ - 16#24 - 36
        ( 16#23#, 16#13#, 16#08#, 16#64#, 16#62# ),  -- % - 16#25 - 37
        ( 16#36#, 16#49#, 16#55#, 16#22#, 16#50# ),  -- & - 16#26 - 38
        ( 16#00#, 16#05#, 16#03#, 16#00#, 16#00# ),  -- ' - 16#27 - 39
        ( 16#00#, 16#1c#, 16#22#, 16#41#, 16#00# ),  -- ( - 16#28 - 40
        ( 16#00#, 16#41#, 16#22#, 16#1c#, 16#00# ),  -- ) - 16#29 - 41
        ( 16#14#, 16#08#, 16#3e#, 16#08#, 16#14# ),  -- * - 16#2a - 42
        ( 16#08#, 16#08#, 16#3e#, 16#08#, 16#08# ),  -- + - 16#2b - 43
        ( 16#00#, 16#50#, 16#30#, 16#00#, 16#00# ),  -- , - 16#2c - 44
        ( 16#08#, 16#08#, 16#08#, 16#08#, 16#08# ),  -- - - 16#2d - 45
        ( 16#00#, 16#60#, 16#60#, 16#00#, 16#00# ),  -- . - 16#2e - 46
        ( 16#20#, 16#10#, 16#08#, 16#04#, 16#02# ),  -- / - 16#2f - 47
        ( 16#3e#, 16#51#, 16#49#, 16#45#, 16#3e# ),  -- 0 - 16#30 - 48
        ( 16#00#, 16#42#, 16#7f#, 16#40#, 16#00# ),  -- 1 - 16#31 - 49
        ( 16#42#, 16#61#, 16#51#, 16#49#, 16#46# ),  -- 2 - 16#32 - 50
        ( 16#21#, 16#41#, 16#45#, 16#4b#, 16#31# ),  -- 3 - 16#33 - 51
        ( 16#18#, 16#14#, 16#12#, 16#7f#, 16#10# ),  -- 4 - 16#34 - 52
        ( 16#27#, 16#45#, 16#45#, 16#45#, 16#39# ),  -- 5 - 16#35 - 53
        ( 16#3c#, 16#4a#, 16#49#, 16#49#, 16#30# ),  -- 6 - 16#36 - 54
        ( 16#01#, 16#71#, 16#09#, 16#05#, 16#03# ),  -- 7 - 16#37 - 55
        ( 16#36#, 16#49#, 16#49#, 16#49#, 16#36# ),  -- 8 - 16#38 - 56
        ( 16#06#, 16#49#, 16#49#, 16#29#, 16#1e# ),  -- 9 - 16#39 - 57
        ( 16#00#, 16#36#, 16#36#, 16#00#, 16#00# ),  -- : - 16#3a - 58
        ( 16#00#, 16#56#, 16#36#, 16#00#, 16#00# ),  -- ; - 16#3b - 59
        ( 16#08#, 16#14#, 16#22#, 16#41#, 16#00# ),  -- < - 16#3c - 60
        ( 16#14#, 16#14#, 16#14#, 16#14#, 16#14# ),  -- = - 16#3d - 61
        ( 16#00#, 16#41#, 16#22#, 16#14#, 16#08# ),  -- > - 16#3e - 62
        ( 16#02#, 16#01#, 16#51#, 16#09#, 16#06# ),  -- ? - 16#3f - 63
        ( 16#32#, 16#49#, 16#79#, 16#41#, 16#3e# ),  -- @ - 16#40 - 64
        ( 16#7e#, 16#11#, 16#11#, 16#11#, 16#7e# ),  -- A - 16#41 - 65
        ( 16#7f#, 16#49#, 16#49#, 16#49#, 16#36# ),  -- B - 16#42 - 66
        ( 16#3e#, 16#41#, 16#41#, 16#41#, 16#22# ),  -- C - 16#43 - 67
        ( 16#7f#, 16#41#, 16#41#, 16#22#, 16#1c# ),  -- D - 16#44 - 68
        ( 16#7f#, 16#49#, 16#49#, 16#49#, 16#41# ),  -- E - 16#45 - 69
        ( 16#7f#, 16#09#, 16#09#, 16#09#, 16#01# ),  -- F - 16#46 - 70
        ( 16#3e#, 16#41#, 16#49#, 16#49#, 16#7a# ),  -- G - 16#47 - 71
        ( 16#7f#, 16#08#, 16#08#, 16#08#, 16#7f# ),  -- H - 16#48 - 72
        ( 16#00#, 16#41#, 16#7f#, 16#41#, 16#00# ),  -- I - 16#49 - 73
        ( 16#20#, 16#40#, 16#41#, 16#3f#, 16#01# ),  -- J - 16#4a - 74
        ( 16#7f#, 16#08#, 16#14#, 16#22#, 16#41# ),  -- K - 16#4b - 75
        ( 16#7f#, 16#40#, 16#40#, 16#40#, 16#40# ),  -- L - 16#4c - 76
        ( 16#7f#, 16#02#, 16#0c#, 16#02#, 16#7f# ),  -- M - 16#4d - 77
        ( 16#7f#, 16#04#, 16#08#, 16#10#, 16#7f# ),  -- N - 16#4e - 78
        ( 16#3e#, 16#41#, 16#41#, 16#41#, 16#3e# ),  -- O - 16#4f - 79
        ( 16#7f#, 16#09#, 16#09#, 16#09#, 16#06# ),  -- P - 16#50 - 80
        ( 16#3e#, 16#41#, 16#51#, 16#21#, 16#5e# ),  -- Q - 16#51 - 81
        ( 16#7f#, 16#09#, 16#19#, 16#29#, 16#46# ),  -- R - 16#52 - 82
        ( 16#46#, 16#49#, 16#49#, 16#49#, 16#31# ),  -- S - 16#53 - 83
        ( 16#01#, 16#01#, 16#7f#, 16#01#, 16#01# ),  -- T - 16#54 - 84
        ( 16#3f#, 16#40#, 16#40#, 16#40#, 16#3f# ),  -- U - 16#55 - 85
        ( 16#1f#, 16#20#, 16#40#, 16#20#, 16#1f# ),  -- V - 16#56 - 86
        ( 16#3f#, 16#40#, 16#38#, 16#40#, 16#3f# ),  -- W - 16#57 - 87
        ( 16#63#, 16#14#, 16#08#, 16#14#, 16#63# ),  -- X - 16#58 - 88
        ( 16#07#, 16#08#, 16#70#, 16#08#, 16#07# ),  -- Y - 16#59 - 89
        ( 16#61#, 16#51#, 16#49#, 16#45#, 16#43# ),  -- Z - 16#5a - 90
        ( 16#00#, 16#7f#, 16#41#, 16#41#, 16#00# ),  -- [ - 16#5b - 91
        ( 16#02#, 16#04#, 16#08#, 16#10#, 16#20# ),  -- \ - 16#5c - 92
        ( 16#00#, 16#41#, 16#41#, 16#7f#, 16#00# ),  -- ] - 16#5d - 93
        ( 16#04#, 16#02#, 16#01#, 16#02#, 16#04# ),  -- ^ - 16#5e - 94
        ( 16#40#, 16#40#, 16#40#, 16#40#, 16#40# ),  -- _ - 16#5f - 95
        ( 16#00#, 16#01#, 16#02#, 16#04#, 16#00# ),  -- ` - 16#60 - 96
        ( 16#20#, 16#54#, 16#54#, 16#54#, 16#78# ),  -- a - 16#61 - 97
        ( 16#7f#, 16#48#, 16#44#, 16#44#, 16#38# ),  -- b - 16#62 - 98
        ( 16#38#, 16#44#, 16#44#, 16#44#, 16#20# ),  -- c - 16#63 - 99
        ( 16#38#, 16#44#, 16#44#, 16#48#, 16#7f# ),  -- d - 16#64 - 100
        ( 16#38#, 16#54#, 16#54#, 16#54#, 16#18# ),  -- e - 16#65 - 101
        ( 16#08#, 16#7e#, 16#09#, 16#01#, 16#02# ),  -- f - 16#66 - 102
        ( 16#38#, 16#44#, 16#44#, 16#54#, 16#34# ),  -- g - 16#67 - 103
        ( 16#7f#, 16#08#, 16#04#, 16#04#, 16#78# ),  -- h - 16#68 - 104
        ( 16#00#, 16#44#, 16#7d#, 16#40#, 16#00# ),  -- i - 16#69 - 105
        ( 16#20#, 16#40#, 16#44#, 16#3d#, 16#00# ),  -- j - 16#6a - 106
        ( 16#7f#, 16#10#, 16#28#, 16#44#, 16#00# ),  -- k - 16#6b - 107
        ( 16#00#, 16#41#, 16#7f#, 16#40#, 16#00# ),  -- l - 16#6c - 108
        ( 16#7c#, 16#04#, 16#18#, 16#04#, 16#78# ),  -- m - 16#6d - 109
        ( 16#7c#, 16#08#, 16#04#, 16#04#, 16#78# ),  -- n - 16#6e - 110
        ( 16#38#, 16#44#, 16#44#, 16#44#, 16#38# ),  -- o - 16#6f - 111
        ( 16#7c#, 16#14#, 16#14#, 16#14#, 16#08# ),  -- p - 16#70 - 112
        ( 16#08#, 16#14#, 16#14#, 16#18#, 16#7c# ),  -- q - 16#71 - 113
        ( 16#7c#, 16#08#, 16#04#, 16#04#, 16#08# ),  -- r - 16#72 - 114
        ( 16#48#, 16#54#, 16#54#, 16#54#, 16#20# ),  -- s - 16#73 - 115
        ( 16#04#, 16#3f#, 16#44#, 16#40#, 16#20# ),  -- t - 16#74 - 116
        ( 16#3c#, 16#40#, 16#40#, 16#20#, 16#7c# ),  -- u - 16#75 - 117
        ( 16#1c#, 16#20#, 16#40#, 16#20#, 16#1c# ),  -- v - 16#76 - 118
        ( 16#3c#, 16#40#, 16#30#, 16#40#, 16#3c# ),  -- w - 16#77 - 119
        ( 16#44#, 16#28#, 16#10#, 16#28#, 16#44# ),  -- x - 16#78 - 120
        ( 16#0c#, 16#50#, 16#50#, 16#50#, 16#3c# ),  -- y - 16#79 - 121
        ( 16#44#, 16#64#, 16#54#, 16#4c#, 16#44# ),  -- z - 16#7a - 122
        ( 16#00#, 16#08#, 16#36#, 16#41#, 16#00# ),  -- { - 16#7b - 123
        ( 16#00#, 16#00#, 16#7f#, 16#00#, 16#00# ),  -- | - 16#7c - 124
        ( 16#00#, 16#41#, 16#36#, 16#08#, 16#00# ),  -- } - 16#7d - 125
        ( 16#10#, 16#08#, 16#08#, 16#10#, 16#08# )   -- ~ - 16#7e - 126
    );


    -- raspberry picture
    raspberry : constant t_lcd_array := (
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 1. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 1. row
        16#00#,16#00#,16#00#,16#00#,16#F0#,16#F8#,16#58#,16#1C#,16#1C#,16#0C#,16#0C#,16#06#,16#86#,16#86#,16#86#,16#0E#, -- 1. row
        16#0E#,16#06#,16#0E#,16#1E#,16#1C#,16#1C#,16#0C#,16#3C#,16#38#,16#78#,16#F0#,16#E0#,16#C0#,16#C0#,16#E0#,16#70#, -- 1. row
        16#38#,16#18#,16#1C#,16#1C#,16#0C#,16#0E#,16#0E#,16#0E#,16#0E#,16#0E#,16#0E#,16#86#,16#86#,16#06#,16#04#,16#0C#, -- 1. row
        16#0C#,16#18#,16#F8#,16#F8#,16#F8#,16#F0#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 1. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 1. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 1. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 2. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 2. row
        16#00#,16#00#,16#00#,16#00#,16#03#,16#0F#,16#3F#,16#7C#,16#F0#,16#C0#,16#C0#,16#C0#,16#C0#,16#00#,16#01#,16#01#, -- 2. row
        16#03#,16#02#,16#06#,16#04#,16#04#,16#1C#,16#B8#,16#F0#,16#E0#,16#70#,16#30#,16#1F#,16#0F#,16#0F#,16#3F#,16#30#, -- 2. row
        16#F0#,16#E0#,16#B0#,16#18#,16#08#,16#0C#,16#04#,16#06#,16#02#,16#01#,16#01#,16#00#,16#00#,16#00#,16#00#,16#C0#, -- 2. row
        16#F0#,16#F8#,16#7F#,16#1F#,16#07#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 2. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 2. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 2. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 3. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 3. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#80#,16#E0#,16#F3#,16#33#,16#3F#,16#1F#,16#1E#,16#0E#,16#0E#, -- 3. row
        16#0C#,16#0C#,16#EC#,16#EC#,16#EE#,16#BE#,16#0F#,16#0F#,16#07#,16#06#,16#02#,16#02#,16#02#,16#02#,16#02#,16#06#, -- 3. row
        16#06#,16#0F#,16#0F#,16#BE#,16#EE#,16#CC#,16#8C#,16#0C#,16#0C#,16#0C#,16#0E#,16#1E#,16#1E#,16#3F#,16#73#,16#E1#, -- 3. row
        16#C1#,16#00#,16#36#,16#25#,16#55#,16#5D#,16#49#,16#6D#,16#32#,16#0C#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 3. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 3. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 3. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 4. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 4. row
        16#00#,16#00#,16#00#,16#80#,16#C0#,16#E0#,16#70#,16#7F#,16#7F#,16#3F#,16#FC#,16#FC#,16#FC#,16#3C#,16#1C#,16#0E#, -- 4. row
        16#07#,16#07#,16#03#,16#03#,16#03#,16#03#,16#03#,16#02#,16#06#,16#0E#,16#1C#,16#FC#,16#FC#,16#FC#,16#3C#,16#0E#, -- 4. row
        16#06#,16#02#,16#03#,16#03#,16#03#,16#03#,16#03#,16#03#,16#07#,16#06#,16#0C#,16#18#,16#78#,16#F0#,16#F0#,16#33#, -- 4. row
        16#7F#,16#7F#,16#70#,16#F0#,16#C0#,16#80#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 4. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 4. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 4. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 5. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 5. row
        16#00#,16#00#,16#FF#,16#FF#,16#83#,16#00#,16#00#,16#00#,16#00#,16#C0#,16#FF#,16#FF#,16#F0#,16#F0#,16#F0#,16#80#, -- 5. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#80#,16#80#,16#C0#,16#60#,16#78#,16#7F#,16#7F#,16#7F#,16#7C#,16#70#, -- 5. row
        16#E0#,16#C0#,16#80#,16#80#,16#00#,16#00#,16#00#,16#00#,16#00#,16#80#,16#80#,16#C0#,16#E0#,16#FF#,16#FF#,16#C0#, -- 5. row
        16#00#,16#00#,16#00#,16#00#,16#C3#,16#FF#,16#FE#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 5. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 5. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 5. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 6. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 6. row
        16#00#,16#00#,16#00#,16#03#,16#0F#,16#3E#,16#FC#,16#FC#,16#0E#,16#03#,16#03#,16#03#,16#03#,16#07#,16#07#,16#0F#, -- 6. row
        16#1F#,16#3F#,16#7F#,16#FF#,16#FF#,16#07#,16#01#,16#01#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 6. row
        16#00#,16#00#,16#01#,16#03#,16#FF#,16#FF#,16#7F#,16#1F#,16#0F#,16#07#,16#07#,16#03#,16#01#,16#01#,16#01#,16#01#, -- 6. row
        16#03#,16#FE#,16#FE#,16#7E#,16#07#,16#03#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 6. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 6. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 6. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 7. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 7. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#01#,16#07#,16#1F#,16#3C#,16#78#,16#70#,16#E0#,16#E0#,16#E0#,16#C0#, -- 7. row
        16#C0#,16#C0#,16#C0#,16#E1#,16#FF#,16#7F#,16#7F#,16#3C#,16#3C#,16#3C#,16#38#,16#30#,16#30#,16#30#,16#30#,16#38#, -- 7. row
        16#38#,16#3C#,16#3C#,16#7F#,16#FF#,16#E1#,16#C0#,16#C0#,16#C0#,16#C0#,16#C0#,16#E0#,16#60#,16#70#,16#38#,16#38#, -- 7. row
        16#1E#,16#0F#,16#03#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 7. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 7. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 7. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 8. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 8. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#01#,16#01#, -- 8. row
        16#01#,16#03#,16#03#,16#07#,16#07#,16#0E#,16#18#,16#18#,16#38#,16#30#,16#30#,16#30#,16#30#,16#30#,16#30#,16#30#, -- 8. row
        16#38#,16#18#,16#1C#,16#1E#,16#0F#,16#07#,16#03#,16#03#,16#01#,16#01#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 8. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 8. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#, -- 8. row
        16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#,16#00#  -- 8. row
    );


    -- lcd init values
    lcd_init_data : constant t_byte_array := (
        16#a0#,  -- cmd8:  adc select
        16#c0#,  -- cmd15: shl select
        16#a3#,  -- cmd11: lcd bias set
        16#2c#,  -- cmd16: power control set (vc=1, vr=0, vf=0)
        16#2e#,  -- cmd16: power control set (vc=1, vr=1, vf=0)
        16#2f#,  -- cmd16: power control set (vc=1, vr=1, vf=1)
        16#26#,  -- cmd17: regulator resistor select
        16#60#,  -- cmd2:  display start line
        16#a6#,  -- cmd6:  display normal
        16#c8#,  -- cmd15: common output mode select (reversed)
        16#af#,  -- cmd1:  display on 
        16#a4#,  -- cmd10: all points off
        16#81#,  -- cmd18: set volume 1st
        16#18#   -- cmd18: set volume 2nd (brightness)
    );


    -- pin definitions
    LCD_CS  : constant Interfaces.C.unsigned_char := 24;
    LCD_RST : constant Interfaces.C.unsigned_char := 23;
    LCD_A0  : constant Interfaces.C.unsigned_char := 22;
    LCD_CLK : constant Interfaces.C.unsigned_char := 27;
    LCD_SI  : constant Interfaces.C.unsigned_char := 17;


    -- procedures declarations
    procedure io_init;
    procedure lcd_init;
    procedure lcd_ascii57_string (xpos : natural; ypos : natural; data : string);
    procedure lcd_ascii57 (xpos : natural; ypos : natural; data : character);
    procedure lcd_picture (xpos : natural; ypos: natural; picture : t_lcd_array);
    procedure lcd_clear;
    procedure lcd_set_page (page : natural; column : natural);
    procedure lcd_transfer_data (value : byte; si : boolean);
    procedure lcd_byte (data : byte);

    function bmp_to_lcd (bmp : t_bmp_array; color_mask : t_color_mask) return t_lcd_array;
    function which_byte (data : dword) return integer;


end st7565lcd;
