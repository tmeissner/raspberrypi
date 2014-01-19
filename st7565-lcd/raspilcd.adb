with Ada.Text_IO;
with bcm2835_h;
with st7565lcd;

with Interfaces; use Interfaces;
with Ada.Text_IO, Ada.Integer_Text_IO, Ada.Streams.Stream_IO;
use Ada.Text_IO, Ada.Integer_Text_IO, Ada.Streams.Stream_IO;
with Ada.Command_Line; use Ada.Command_Line;


procedure raspilcd is


    -- shorter names for packages
    package IO  renames Ada.Text_IO;
    package LCD renames st7565lcd;
    package IOS renames Ada.Streams.Stream_IO;

    -- stream.io definitions
    My_File        : IOS.FILE_TYPE;
    My_File_Access : IOS.STREAM_ACCESS;

    -- picture data
    picture_header  : LCD.t_bmp_header;
    color_mask      : LCD.t_color_mask;
    picture_data    : LCD.t_bmp_array (0 .. 128 * 64 - 1);

    -- lcd pixel array
    lcd_data : LCD.t_lcd_array;

    -- exception handling
    tool_info : string := "raspi-lcd version 0.1, (c) 2014 by tmeissner";
    usage     : string := "usage: ./raspitest BMP-FILE (as root)";
    bmp_error : string := "error: malformed BMP-FILE (valid: 128x64, no compression, 32bpp)";
    cli_exception : exception;
    bmp_exception : exception;


begin


    -- no picture given
    if Argument_Count /= 1 then
        raise cli_exception;
    end if;

    -- open picture file
    declare
        filename : string := Argument (1);
    begin
        IOS.Open(My_File, In_File, filename);
        My_File_Access := IOS.Stream(My_File);
    end;


    -- read bmp header
    LCD.t_bmp_header'Read(My_File_Access, picture_header);

    --Put_Line("Width:       " & Integer'Image(picture_header.biWidth));
    --Put_Line("Height:      " & Integer'Image(picture_header.biHeight));
    --Put_Line("Color Depth: " & Integer'Image(Integer(picture_header.biBitCount)));
    --Put_Line("Compression: " & Integer'Image(Integer(picture_header.biCompression)));

    -- check for valid bmp format    
    if (abs picture_header.biHeight /= 64 or picture_header.biWidth /= 128 or 
        (picture_header.biCompression /= 0 and picture_header.biCompression /= 3) or
        picture_header.biBitCount /= 32) then
        raise bmp_exception;
    end if;

    -- get color map if existing
    if picture_header.biCompression = 3 then
        LCD.t_color_mask'Read(My_File_Access, color_mask);
    end if;
   
    -- read in image data
    if picture_header.biHeight < 0 then

        -- top-down pixel matrix
        for index in picture_data'range loop
            if not IOS.End_Of_File(My_File) then
                LCD.t_byte_array'Read(My_File_Access, picture_data(index));
            end if;
        end loop;

    else

        -- bottom-top pixel matrix
        for row in reverse 0 .. 63 loop
            for column in 0 .. 127 loop
                if not IOS.End_Of_File(My_File) then
                    LCD.t_byte_array'Read(My_File_Access, picture_data(row * 128 + column));
                end if;
            end loop;
        end loop;

    end if;
    
    -- close picture file
    Ada.Streams.Stream_IO.Close(My_File);
    
    -- convert bmp to lcd matrix
    lcd_data := LCD.bmp_to_lcd(bmp => picture_data, color_mask => color_mask);


    -- load bcm2835 lib
    -- print picture and some text on lcd
    if integer(bcm2835_h.bcm2835_init) = 0 then

        IO.Put_Line("Error while initializing BCM2835 library");
    
    else

        LCD.io_init;
 
        LCD.lcd_init;
 
        LCD.lcd_picture(xpos => 0, ypos => 0, picture => lcd_data);

        bcm2835_h.bcm2835_delay(5000);
 
        LCD.lcd_clear;

        LCD.lcd_ascii57_string(xpos => 0, ypos => 0, data => "raspiFPGA 0.1");
        LCD.lcd_ascii57_string(xpos => 0, ypos => 1, data => "(c) raspiDEV 2013");

        -- close library
        if integer(bcm2835_h.bcm2835_close) = 0 then
            IO.Put_Line("Error while closing BCM2835 library");
        end if;

    end if;


    -- exception handling
    exception
    when e: cli_exception =>
        put_line(tool_info);
        put_line(usage);
    when e: bmp_exception =>
        put_line(tool_info);
        put_line(bmp_error);


end raspilcd;
