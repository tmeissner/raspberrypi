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
use Interfaces;
with Interfaces.C;
use Interfaces.C;
with Interfaces.C.extensions;
use Interfaces.C.extensions;
with bcm2835_h;
use bcm2835_h;
with Ada.Text_IO;
use Ada.Text_IO;



package body st7565lcd is


    procedure io_init is
    begin
        bcm2835_gpio_fsel(pin => LCD_CS, mode => unsigned_char(BCM2835_GPIO_FSEL_OUTP));
        bcm2835_gpio_fsel(pin => LCD_RST, mode => unsigned_char(BCM2835_GPIO_FSEL_OUTP));
        bcm2835_gpio_fsel(pin => LCD_A0, mode => unsigned_char(BCM2835_GPIO_FSEL_OUTP));
        bcm2835_gpio_fsel(pin => LCD_CLK, mode => unsigned_char(BCM2835_GPIO_FSEL_OUTP));
        bcm2835_gpio_fsel(pin => LCD_SI, mode => unsigned_char(BCM2835_GPIO_FSEL_OUTP));
    end io_init;


    procedure lcd_init is
    begin
        -- reset
        bcm2835_gpio_write(pin => LCD_CS, on => unsigned_char(HIGH));
        bcm2835_delayMicroseconds(unsigned_long_long(1));
        bcm2835_gpio_write(pin => LCD_RST, on => unsigned_char(LOW));
        bcm2835_delayMicroseconds(unsigned_long_long(1));
        bcm2835_gpio_write(pin => LCD_RST, on => unsigned_char(HIGH));
        bcm2835_delayMicroseconds(unsigned_long_long(1));
        -- init routine
        for index in lcd_init_data'range loop
            lcd_transfer_data(value => lcd_init_data(index), si => false);
        end loop;
        lcd_clear;
    end lcd_init;


    -- display strings on the lcd at a given position
    -- data is a string with any allowed range
    -- for strings not beginning at 0 we have to decrement the index
    -- variable for xpos by the value of the range begin, so we get
    -- for xpos a range from (xpos + 0 .. xpos + number of char in string)  
    procedure lcd_ascii57_string (xpos : natural; ypos : natural; data : string) is
    begin
        for index in data'range loop
            lcd_ascii57(xpos => xpos + (index - data'first) * 6, ypos => ypos, data => character'val(character'pos(data(index))));
        end loop;
    end lcd_ascii57_string;


    procedure lcd_ascii57 (xpos : natural; ypos : natural; data : character) is
    begin
        lcd_set_page(page => ypos, column => xpos);
        -- write one 5x7 char
        for index in 0..4 loop
            lcd_transfer_data(value => font_5x7(character'pos(data))(index), si => true);
        end loop;
        -- one free column between chars
        lcd_transfer_data(value => 16#00#, si => true);
    end lcd_ascii57;


    procedure lcd_picture (xpos : natural; ypos: natural; picture : t_lcd_array) is
    begin
        for outdex in 0..7 loop 
            lcd_set_page(page => ypos + outdex, column => xpos);
            for index in (128 * outdex) .. (128 * (outdex + 1) - 1) loop
              lcd_transfer_data(value => picture(index), si => true);
            end loop;
        end loop;
    end lcd_picture;


    procedure lcd_clear is
    begin
        bcm2835_gpio_write(pin => LCD_CS, on => unsigned_char(LOW));
        for outdex in 0..7 loop
            lcd_set_page(page => outdex, column => 0);
            for index in 0..128 loop
                lcd_transfer_data(value => 16#00#, si => true);
            end loop;
        end loop;
        bcm2835_gpio_write(pin => LCD_CS, on => unsigned_char(HIGH));
    end lcd_clear;


    procedure lcd_transfer_data (value : byte; si : boolean) is
    begin
        bcm2835_gpio_write(pin => LCD_CS, on => unsigned_char(LOW));
        bcm2835_gpio_write(pin => LCD_CLK, on => unsigned_char(HIGH));
        if si then
            bcm2835_gpio_write(pin => LCD_A0, on => unsigned_char(HIGH));
        else
            bcm2835_gpio_write(pin => LCD_A0, on => unsigned_char(LOW));
        end if;
        lcd_byte(value);
        bcm2835_gpio_write(pin => LCD_CS, on => unsigned_char(HIGH));
    end lcd_transfer_data;


    procedure lcd_set_page (page : natural; column : natural) is
        lsb      : byte := byte(column + 1) and 16#0f#;
        msb      : byte := byte(column + 1) and 16#f0#;
        page_int : byte := byte(page)   or  16#b0#;
    begin
        msb := Shift_Right(msb, 4);
        msb := msb or 16#10#;
        lcd_transfer_data(value => page_int, si => false);
        lcd_transfer_data(value => msb, si => false);
        lcd_transfer_data(value => lsb, si => false);
        null;
    end lcd_set_page;


    procedure lcd_byte (data : byte) is
        data_int : byte := data;
    begin
        for index in 0..7 loop
            bcm2835_delayMicroseconds(unsigned_long_long(1));
            bcm2835_gpio_write(pin => LCD_CLK, on => unsigned_char(LOW));
            if (data_int and 16#80#) = 16#80# then
                bcm2835_gpio_write(pin => LCD_SI, on => unsigned_char(HIGH));
            else
                bcm2835_gpio_write(pin => LCD_SI, on => unsigned_char(LOW));
            end if;
            data_int := Shift_Left(data_int, 1);
            bcm2835_delayMicroseconds(unsigned_long_long(1));
            bcm2835_gpio_write(pin => LCD_CLK, on => unsigned_char(HIGH));
        end loop;
    end lcd_byte;


    function bmp_to_lcd (bmp_picture : t_bmp_picture) return t_lcd_array is
        lcd   : t_lcd_array := (others => 16#00#);
        logic : byte;
    begin
        for aussen in 0 .. 7 loop
           logic := 16#01#;
           for outdex in 0 .. 7 loop
              for index in 0 .. 127 loop
                 if ((bmp_picture.data(aussen * 1024 + outdex * 128 + index)(which_byte(bmp_picture.mask.red)) or
                      bmp_picture.data(aussen * 1024 + outdex * 128 + index)(which_byte(bmp_picture.mask.green)) or
                      bmp_picture.data(aussen * 1024 + outdex * 128 + index)(which_byte(bmp_picture.mask.blue))) < 16#88#) then
                    lcd(aussen * 128 + index) := lcd(aussen * 128 + index) or logic;
                 end if;
              end loop;
              logic := Shift_Left(logic, 1);
           end loop;
        end loop;
        return lcd;
    end bmp_to_lcd;


    function which_byte (data : dword) return integer is
    begin
        case data is
            when 16#000000FF# =>
                return 0;
            when 16#0000FF00# =>
                return 1;
            when 16#00FF0000# =>
                return 2;
            when 16#FF000000# =>
                return 3;
            when others =>
                raise mask_exception;
        end case;
    end which_byte;


    -- read_bmp : read a bmp file in t_bmp_picture record
    procedure read_bmp (file        : in     Ada.Streams.Stream_IO.File_Type;
                        file_access : access Ada.Streams.Root_Stream_Type'Class;
                        bmp_picture : in out t_bmp_picture) is
    begin
        -- read header
        t_bmp_header'Read(file_access, bmp_picture.header);
        -- check for valid header
        if (abs bmp_picture.header.biHeight /= 64 or bmp_picture.header.biWidth /= 128 or 
                (bmp_picture.header.biCompression /= 0 and bmp_picture.header.biCompression /= 3) or
                bmp_picture.header.biBitCount /= 32) then
            raise bmp_exception;
        end if;
        -- get color map if existing
        if bmp_picture.header.biCompression = 3 then
            t_color_mask'Read(file_access, bmp_picture.mask);
        end if;
        -- read in image data
        if bmp_picture.header.biHeight < 0 then
            -- top-down pixel matrix
            for index in bmp_picture.data'range loop
                if not Ada.Streams.Stream_IO.End_Of_File(file) then
                    t_byte_array'Read(file_access, bmp_picture.data(index));
                end if;
            end loop;
        else
            -- bottom-top pixel matrix
            for row in reverse 0 .. 63 loop
                for column in 0 .. 127 loop
                    if not Ada.Streams.Stream_IO.End_Of_File(file) then
                        t_byte_array'Read(file_access, bmp_picture.data(row * 128 + column));
                    end if;
                end loop;
            end loop;
        end if;
    end read_bmp;


end st7565lcd;
