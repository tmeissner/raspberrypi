with Ada.Text_IO;
with bcm2835_h;
with RaspiLcd;



procedure raspitest is


    package IO    renames Ada.Text_IO;
    package LCD renames RaspiLcd;


begin


    if integer(bcm2835_h.bcm2835_init) = 0 then

        IO.Put_Line("Error while initializing BCM2835 library");
    
    else

        LCD.io_init;
 
        LCD.lcd_init;
 
        LCD.lcd_picture(xpos => 0, ypos => 0);

        bcm2835_h.bcm2835_delay(5000);
 
        LCD.lcd_clear;

        LCD.lcd_ascii57_string(xpos => 0, ypos => 0, data => "raspiFPGA 0.1");
        LCD.lcd_ascii57_string(xpos => 0, ypos => 1, data => "(c) raspiDEV 2013");

        -- close library
        if integer(bcm2835_h.bcm2835_close) = 0 then
            IO.Put_Line("Error while closing BCM2835 library");
        end if;

    end if;


end raspitest;