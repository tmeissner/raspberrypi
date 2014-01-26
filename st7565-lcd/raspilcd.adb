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



with Ada.Text_IO;
with bcm2835_h;
with st7565lcd;

with Interfaces;
use Interfaces;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Streams.Stream_IO;
with Ada.Command_Line;
use Ada.Command_Line;



procedure raspilcd is


    -- shorter names for packages
    package IO  renames Ada.Text_IO;
    package LCD renames st7565lcd;
    package IOS renames Ada.Streams.Stream_IO;

    -- stream.io definitions
    My_File        : IOS.FILE_TYPE;
    My_File_Access : IOS.STREAM_ACCESS;

    -- picture data
    bmp_picture : LCD.t_bmp_picture;

    -- lcd pixel array
    lcd_data : LCD.t_lcd_array;

    -- exception handling
    cli_exception : exception;


begin


    -- no picture given
    if Argument_Count /= 1 then
        raise cli_exception;
    end if;

    -- open picture file
    declare
        filename : string := Argument(1);
    begin
        IOS.Open(My_File, IOS.In_File, filename);
        My_File_Access := IOS.Stream(My_File);
    end;

    -- read in picture
    LCD.read_bmp(file => My_File, file_access => My_File_Access, bmp_picture => bmp_picture);

    --Put_Line("Width:       " & Integer'Image(picture_header.biWidth));
    --Put_Line("Height:      " & Integer'Image(picture_header.biHeight));
    --Put_Line("Color Depth: " & Integer'Image(Integer(picture_header.biBitCount)));
    --Put_Line("Compression: " & Integer'Image(Integer(picture_header.biCompression)));

    -- close picture file
    Ada.Streams.Stream_IO.Close(My_File);
    
    -- convert bmp to lcd matrix
    lcd_data := LCD.bmp_to_lcd(bmp_picture);


    -- load bcm2835 lib
    -- print picture and some text on lcd
    if integer(bcm2835_h.bcm2835_init) = 0 then

        IO.Put_Line("Error while initializing BCM2835 library");
    
    else

        LCD.io_init;
 
        LCD.lcd_init;
 
        LCD.lcd_picture(xpos => 0, ypos => 0, picture => lcd_data);

        --bcm2835_h.bcm2835_delay(5000);
 
        --LCD.lcd_clear;

        --LCD.lcd_ascii57_string(xpos => 0, ypos => 0, data => "raspiFPGA 0.1");
        --LCD.lcd_ascii57_string(xpos => 0, ypos => 1, data => "(c) raspiDEV 2013");

        -- close library
        if integer(bcm2835_h.bcm2835_close) = 0 then
            IO.Put_Line("Error while closing BCM2835 library");
        end if;

    end if;


    -- exception handling
    exception
        when cli_exception =>
            put_line(LCD.exception_head);
            put_line("usage: ./raspitest BMP-FILE (as root)");
        when LCD.bmp_exception =>
            put_line(LCD.exception_head);
            put_line("error: malformed BMP-FILE (valid: 128x64, no compression, 32bpp)");
        when LCD.mask_exception =>
            put_line(LCD.exception_head);
            put_line("error: malformed BMP color mask");


end raspilcd;
