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
    cli_exception     : exception;


begin


    -- command line argument error
    if Argument_Count = 0 or Argument_Count > 2 or
       (Argument_Count = 1 and Argument(1)  = "-i") or
       (Argument_Count = 2 and Argument(1) /= "-i")  then
        raise cli_exception;
    end if;

    -- open picture file
    declare
        filename : string := Argument(Argument_Count);
    begin
        IOS.Open(My_File, IOS.In_File, filename);
        My_File_Access := IOS.Stream(My_File);
    end;

    -- read in picture
    LCD.read_bmp(file => My_File, file_access => My_File_Access, bmp_picture => bmp_picture);

    -- print bmp header info
    if Argument(1) = "-i" then
        put_Line("  width:       " & Integer'Image(bmp_picture.header.biWidth));
        put_Line("  height:      " & Integer'Image(bmp_picture.header.biHeight));
        put_Line("  color depth: " & Integer'Image(Integer(bmp_picture.header.biBitCount)));
        put_Line("  compression: " & Integer'Image(Integer(bmp_picture.header.biCompression)));
    end if;

    -- close picture file
    Ada.Streams.Stream_IO.Close(My_File);
    
    -- convert bmp to lcd matrix
    lcd_data := LCD.bmp_to_lcd(bmp_picture);


    -- load bcm2835 lib
    -- print picture and some text on lcd
    if integer(bcm2835_h.bcm2835_init) /= 0 then

        LCD.io_init;
 
        LCD.lcd_init;
 
        LCD.lcd_picture(xpos => 0, ypos => 0, picture => lcd_data);

        --bcm2835_h.bcm2835_delay(5000);
 
        --LCD.lcd_clear;

        --LCD.lcd_ascii57_string(xpos => 0, ypos => 0, data => "raspiFPGA 0.1");
        --LCD.lcd_ascii57_string(xpos => 0, ypos => 1, data => "(c) raspiDEV 2013");

        -- close library
        if integer(bcm2835_h.bcm2835_close) = 0 then
            put_line("Error while closing BCM2835 library");
        end if;

    end if;


    -- exception handling
    exception
        when cli_exception | CONSTRAINT_ERROR =>
            put_line(LCD.exception_head);
            put_line("usage: ./raspilcd [option] BMP-FILE (as root)");
            put_line("   -i: show bmp info");
        when LCD.bmp_exception =>
            put_line(LCD.exception_head);
            put_line("error: malformed BMP-FILE (valid: 128x64, no compression, 32bpp)");
        when LCD.mask_exception =>
            put_line(LCD.exception_head);
            put_line("error: malformed BMP color mask");
        when NAME_ERROR =>
            put_line(LCD.exception_head);
            put_line("error: Could not find bmp file");


end raspilcd;
