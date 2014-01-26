-- BCM2835 library Ada bindings
-- derived from /usr/local/include/bcm2835.h
-- with the GNAT compiler:
--
-- $ g++ -c -fdump-ada-spec -C /usr/local/include/bcm2835.h
-- $ gcc -c -gnat05 *.ads
-- 
-- see for more at: http://www.adacore.com/adaanswers/gems/gem-59/#sthash.eDZ2bNEb.dpuf
--
-- we get two files:
-- bcm2835_h.ads -> bcm2835.h bindings
-- stdint_h.ads  -> stdint.h bindings
--
-- some #defines couldn't be transformed by GNAT, so we have to do that manually
-- they are marked and commented out in the generated file
-- I've converted them by myself in this file, they are marked with the
-- 'manually converted' comment before (first expressions after the big comment block at the bgin of the package)




with Interfaces.C; use Interfaces.C;
with stdint_h;
with Interfaces.C.Strings;


package bcm2835_h is

  -- bcm2835.h
  -- C and C++ support for Broadcom BCM 2835 as used in Raspberry Pi
  -- Author: Mike McCauley
  -- Copyright (C) 2011-2013 Mike McCauley
  -- $Id: bcm2835.h,v 1.13 2013/12/06 22:24:52 mikem Exp mikem $
  --/ \mainpage C library for Broadcom BCM 2835 as used in Raspberry Pi
  --/
  --/ This is a C library for Raspberry Pi (RPi). It provides access to 
  --/ GPIO and other IO functions on the Broadcom BCM 2835 chip,
  --/ allowing access to the GPIO pins on the
  --/ 26 pin IDE plug on the RPi board so you can control and interface with various external devices.
  --/
  --/ It provides functions for reading digital inputs and setting digital outputs, using SPI and I2C,
  --/ and for accessing the system timers.
  --/ Pin event detection is supported by polling (interrupts are not supported).
  --/
  --/ It is C++ compatible, and installs as a header file and non-shared library on 
  --/ any Linux-based distro (but clearly is no use except on Raspberry Pi or another board with 
  --/ BCM 2835).
  --/
  --/ The version of the package that this documentation refers to can be downloaded 
  --/ from http://www.airspayce.com/mikem/bcm2835/bcm2835-1.36.tar.gz
  --/ You can find the latest version at http://www.airspayce.com/mikem/bcm2835
  --/
  --/ Several example programs are provided.
  --/
  --/ Based on data in http://elinux.org/RPi_Low-level_peripherals and 
  --/ http://www.raspberrypi.org/wp-content/uploads/2012/02/BCM2835-ARM-Peripherals.pdf
  --/ and http://www.scribd.com/doc/101830961/GPIO-Pads-Control2
  --/
  --/ You can also find online help and discussion at http://groups.google.com/group/bcm2835
  --/ Please use that group for all questions and discussions on this topic. 
  --/ Do not contact the author directly, unless it is to discuss commercial licensing.
  --/
  --/ Tested on debian6-19-04-2012, 2012-07-15-wheezy-raspbian, 2013-07-26-wheezy-raspbian
  --/ and Occidentalisv01
  --/ CAUTION: it has been observed that when detect enables such as bcm2835_gpio_len() 
  --/ are used and the pin is pulled LOW
  --/ it can cause temporary hangs on 2012-07-15-wheezy-raspbian, 2013-07-26-wheezy-raspbian
  --/ and Occidentalisv01.
  --/ Reason for this is not yet determined, but we suspect that an interrupt handler is
  --/ hitting a hard loop on those OSs.
  --/ If you must use bcm2835_gpio_len() and friends, make sure you disable the pins with 
  --/ bcm2835_gpio_clr_len() and friends after use. 
  --/
  --/ \par Installation
  --/
  --/ This library consists of a single non-shared library and header file, which will be
  --/ installed in the usual places by make install
  --/
  --/ \code
  --/ # download the latest version of the library, say bcm2835-1.xx.tar.gz, then:
  --/ tar zxvf bcm2835-1.xx.tar.gz
  --/ cd bcm2835-1.xx
  --/ ./configure
  --/ make
  --/ sudo make check
  --/ sudo make install
  --/ \endcode
  --/
  --/ \par Physical Addresses
  --/
  --/ The functions bcm2835_peri_read(), bcm2835_peri_write() and bcm2835_peri_set_bits() 
  --/ are low level peripheral register access functions. They are designed to use
  --/ physical addresses as described in section 1.2.3 ARM physical addresses
  --/ of the BCM2835 ARM Peripherals manual. 
  --/ Physical addresses range from 0x20000000 to 0x20FFFFFF for peripherals. The bus
  --/ addresses for peripherals are set up to map onto the peripheral bus address range starting at
  --/ 0x7E000000. Thus a peripheral advertised in the manual at bus address 0x7Ennnnnn is available at
  --/ physical address 0x20nnnnnn.
  --/
  --/ The base address of the various peripheral registers are available with the following
  --/ externals:
  --/ bcm2835_gpio
  --/ bcm2835_pwm
  --/ bcm2835_clk
  --/ bcm2835_pads
  --/ bcm2835_spio0
  --/ bcm2835_st
  --/ bcm2835_bsc0
  --/ bcm2835_bsc1
  --/
  --/ \par Pin Numbering
  --/
  --/ The GPIO pin numbering as used by RPi is different to and inconsistent with the underlying 
  --/ BCM 2835 chip pin numbering. http://elinux.org/RPi_BCM2835_GPIOs
  --/ 
  --/ RPi has a 26 pin IDE header that provides access to some of the GPIO pins on the BCM 2835,
  --/ as well as power and ground pins. Not all GPIO pins on the BCM 2835 are available on the 
  --/ IDE header.
  --/
  --/ RPi Version 2 also has a P5 connector with 4 GPIO pins, 5V, 3.3V and Gnd.
  --/
  --/ The functions in this library are designed to be passed the BCM 2835 GPIO pin number and _not_ 
  --/ the RPi pin number. There are symbolic definitions for each of the available pins
  --/ that you should use for convenience. See \ref RPiGPIOPin.
  --/
  --/ \par SPI Pins
  --/ 
  --/ The bcm2835_spi_* functions allow you to control the BCM 2835 SPI0 interface, 
  --/ allowing you to send and received data by SPI (Serial Peripheral Interface).
  --/ For more information about SPI, see http://en.wikipedia.org/wiki/Serial_Peripheral_Interface_Bus
  --/
  --/ When bcm2835_spi_begin() is called it changes the bahaviour of the SPI interface pins from their 
  --/ default GPIO behaviour in order to support SPI. While SPI is in use, you will not be able 
  --/ to control the state of the SPI pins through the usual bcm2835_spi_gpio_write().
  --/ When bcm2835_spi_end() is called, the SPI pins will all revert to inputs, and can then be
  --/ configured and controled with the usual bcm2835_gpio_* calls.
  --/
  --/ The Raspberry Pi GPIO pins used for SPI are:
  --/ 
  --/ - P1-19 (MOSI)
  --/ - P1-21 (MISO) 
  --/ - P1-23 (CLK) 
  --/ - P1-24 (CE0) 
  --/ - P1-26 (CE1)
  --/
  --/ \par I2C Pins
  --/
  --/ The bcm2835_i2c_* functions allow you to control the BCM 2835 BSC interface,
  --/ allowing you to send and received data by I2C ("eye-squared cee"; generically referred to as "two-wire interface") .
  --/ For more information about I?C, see http://en.wikipedia.org/wiki/I%C2%B2C
  --/
  --/ The Raspberry Pi V2 GPIO pins used for I2C are:
  --/
  --/ - P1-03 (SDA)
  --/ - P1-05 (SLC)
  --/
  --/ \par PWM
  --/
  --/ The BCM2835 supports hardware PWM on a limited subset of GPIO pins. This bcm2835 library provides 
  --/ functions for configuring and controlling PWM output on these pins.
  --/
  --/ The BCM2835 contains 2 independent PWM channels (0 and 1), each of which be connnected to a limited subset of 
  --/ GPIO pins. The following GPIO pins may be connected to the following PWM channels (from section 9.5):
  --/ \code
  --/ GPIO PIN    RPi pin  PWM Channel    ALT FUN
  --/    12                    0            0
  --/    13                    1            0
  --/    18         1-12       0            5
  --/    19                    1            5
  --/    40                    0            0
  --/    41                    1            0
  --/    45                    1            0
  --/    52                    0            1
  --/    53                    1            1
  --/ \endcode
  --/ In order for a GPIO pin to emit output from its PWM channel, it must be set to the Alt Function given above.
  --/ Note carefully that current versions of the Raspberry Pi only expose one of these pins (GPIO 18 = RPi Pin 1-12)
  --/ on the IO headers, and therefore this is the only IO pin on the RPi that can be used for PWM.
  --/ Further it must be set to ALT FUN 5 to get PWM output.
  --/
  --/ Both PWM channels are driven by the same PWM clock, whose clock dvider can be varied using 
  --/ bcm2835_pwm_set_clock(). Each channel can be separately enabled with bcm2835_pwm_set_mode().
  --/ The average output of the PWM channel is determined by the ratio of DATA/RANGE for that channel.
  --/ Use bcm2835_pwm_set_range() to set the range and bcm2835_pwm_set_data() to set the data in that ratio
  --/
  --/ Each PWM channel can run in either Balanced or Mark-Space mode. In Balanced mode, the hardware 
  --/ sends a combination of clock pulses that results in an overall DATA pulses per RANGE pulses.
  --/ In Mark-Space mode, the hardware sets the output HIGH for DATA clock pulses wide, followed by 
  --/ LOW for RANGE-DATA clock pulses. 
  --/
  --/ The PWM clock can be set to control the PWM pulse widths. The PWM clock is derived from 
  --/ a 19.2MHz clock. You can set any divider, but some common ones are provided by the BCM2835_PWM_CLOCK_DIVIDER_*
  --/ values of \ref bcm2835PWMClockDivider.
  --/ 
  --/ For example, say you wanted to drive a DC motor with PWM at about 1kHz, 
  --/ and control the speed in 1/1024 increments from 
  --/ 0/1024 (stopped) through to 1024/1024 (full on). In that case you might set the 
  --/ clock divider to be 16, and the RANGE to 1024. The pulse repetition frequency will be
  --/ 1.2MHz/1024 = 1171.875Hz.
  --/
  --/ \par Real Time performance constraints
  --/
  --/ The bcm2835 is a library for user programs (i.e. they run in 'userland'). 
  --/ Such programs are not part of the kernel and are usually
  --/ subject to paging and swapping by the kernel while it does other things besides running your program. 
  --/ This means that you should not expect to get real-time performance or 
  --/ real-time timing constraints from such programs. In particular, there is no guarantee that the 
  --/ bcm2835_delay() and bcm2835_delayMicroseconds() will return after exactly the time requested. 
  --/ In fact, depending on other activity on the host, IO etc, you might get significantly longer delay times
  --/ than the one you asked for. So please dont expect to get exactly the time delay you request.
  --/
  --/ Arjan reports that you can prevent swapping on Linux with the following code fragment:
  --/
  --/ \code
  --/  struct sched_param sp;
  --/  memset(&sp, 0, sizeof(sp));
  --/  sp.sched_priority = sched_get_priority_max(SCHED_FIFO);
  --/  sched_setscheduler(0, SCHED_FIFO, &sp);
  --/  mlockall(MCL_CURRENT | MCL_FUTURE);
  --/ \endcode
  --/
  --/ \par Bindings to other languages
  --/
  --/ mikem has made Perl bindings available at CPAN:
  --/  http://search.cpan.org/~mikem/Device-BCM2835-1.9/lib/Device/BCM2835.pm
  --/ Matthew Baker has kindly made Python bindings available at:
  --/  https://github.com/mubeta06/py-libbcm2835
  --/ Gary Marks has created a Serial Peripheral Interface (SPI) command-line utility 
  --/ for Raspberry Pi, based on the bcm2835 library. The 
  --/ utility, spincl, is licensed under Open Source GNU GPLv3 by iP Solutions (http://ipsolutionscorp.com), as a 
  --/ free download with source included: http://ipsolutionscorp.com/raspberry-pi-spi-utility/
  --/
  --/ \par Open Source Licensing GPL V2
  --/
  --/ This is the appropriate option if you want to share the source code of your
  --/ application with everyone you distribute it to, and you also want to give them
  --/ the right to share who uses it. If you wish to use this software under Open
  --/ Source Licensing, you must contribute all your source code to the open source
  --/ community in accordance with the GPL Version 2 when your application is
  --/ distributed. See http://www.gnu.org/copyleft/gpl.html and COPYING
  --/
  --/ \par Acknowledgements
  --/
  --/ Some of this code has been inspired by Dom and Gert.
  --/ The I2C code has been inspired by Alan Barr.
  --/ 
  --/ \par Revision History
  --/
  --/ \version 1.0 Initial release
  --/ \version 1.1 Minor bug fixes
  --/ \version 1.2 Added support for SPI
  --/ \version 1.3 Added bcm2835_spi_transfern()
  --/ \version 1.4 Fixed a problem that prevented SPI CE1 being used. Reported by David Robinson.
  --/ \version 1.5 Added bcm2835_close() to deinit the library. Suggested by C?sar Ortiz
  --/ \version 1.6 Document testing on 2012-07-15-wheezy-raspbian and Occidentalisv01
  --/              Functions bcm2835_gpio_ren(), bcm2835_gpio_fen(), bcm2835_gpio_hen()
  --/               bcm2835_gpio_len(), bcm2835_gpio_aren() and bcm2835_gpio_afen() now 
  --/               changes only the pin specified. Other pins that were already previously
  --/               enabled stay enabled.
  --/              Added  bcm2835_gpio_clr_ren(), bcm2835_gpio_clr_fen(), bcm2835_gpio_clr_hen()
  --/                bcm2835_gpio_clr_len(), bcm2835_gpio_clr_aren(), bcm2835_gpio_clr_afen() 
  --/                to clear the enable for individual pins, suggested by Andreas Sundstrom.
  --/ \version 1.7 Added bcm2835_spi_transfernb to support different buffers for read and write.
  --/ \version 1.8 Improvements to read barrier, as suggested by maddin.
  --/ \version 1.9 Improvements contributed by mikew: 
  --/              I noticed that it was mallocing memory for the mmaps on /dev/mem.
  --/              It's not necessary to do that, you can just mmap the file directly,
  --/              so I've removed the mallocs (and frees).
  --/              I've also modified delayMicroseconds() to use nanosleep() for long waits,
  --/              and a busy wait on a high resolution timer for the rest. This is because
  --/              I've found that calling nanosleep() takes at least 100-200 us.
  --/              You need to link using '-lrt' using this version.
  --/              I've added some unsigned casts to the debug prints to silence compiler
  --/              warnings I was getting, fixed some typos, and changed the value of
  --/              BCM2835_PAD_HYSTERESIS_ENABLED to 0x08 as per Gert van Loo's doc at
  --/              http://www.scribd.com/doc/101830961/GPIO-Pads-Control2
  --/              Also added a define for the passwrd value that Gert says is needed to
  --/              change pad control settings.
  --/ \version 1.10 Changed the names of the delay functions to bcm2835_delay() 
  --/              and bcm2835_delayMicroseconds() to prevent collisions with wiringPi.
  --/              Macros to map delay()-> bcm2835_delay() and
  --/              Macros to map delayMicroseconds()-> bcm2835_delayMicroseconds(), which
  --/              can be disabled by defining BCM2835_NO_DELAY_COMPATIBILITY
  --/ \version 1.11 Fixed incorrect link to download file
  --/ \version 1.12 New GPIO pin definitions for RPi version 2 (which has a different GPIO mapping)             
  --/ \version 1.13 New GPIO pin definitions for RPi version 2 plug P5
  --/               Hardware base pointers are now available (after initialisation) externally as bcm2835_gpio
  --/               bcm2835_pwm bcm2835_clk bcm2835_pads bcm2835_spi0.
  --/ \version 1.14 Now compiles even if CLOCK_MONOTONIC_RAW is not available, uses CLOCK_MONOTONIC instead.
  --/               Fixed errors in documentation of SPI divider frequencies based on 250MHz clock. 
  --/               Reported by Ben Simpson.
  --/ \version 1.15 Added bcm2835_close() to end of examples as suggested by Mark Wolfe.
  --/ \version 1.16 Added bcm2835_gpio_set_multi, bcm2835_gpio_clr_multi and bcm2835_gpio_write_multi
  --/               to allow a mask of pins to be set all at once. Requested by Sebastian Loncar.
  --/ \version 1.17  Added bcm2835_gpio_write_mask. Requested by Sebastian Loncar.
  --/ \version 1.18 Added bcm2835_i2c_* functions. Changes to bcm2835_delayMicroseconds: 
  --/               now uses the RPi system timer counter, instead of clock_gettime, for improved accuracy. 
  --/               No need to link with -lrt now. Contributed by Arjan van Vught.
  --/ \version 1.19 Removed inlines added by previous patch since they don't seem to work everywhere. 
  --/               Reported by olly.
  --/ \version 1.20 Patch from Mark Dootson to close /dev/mem after access to the peripherals has been granted.
  --/ \version 1.21 delayMicroseconds is now not susceptible to 32 bit timer overruns. 
  --/               Patch courtesy Jeremy Mortis.
  --/ \version 1.22 Fixed incorrect definition of BCM2835_GPFEN0 which broke the ability to set 
  --/               falling edge events. Reported by Mark Dootson.
  --/ \version 1.23 Added bcm2835_i2c_set_baudrate and bcm2835_i2c_read_register_rs. 
  --/               Improvements to bcm2835_i2c_read and bcm2835_i2c_write functions
  --/               to fix ocasional reads not completing. Patched by Mark Dootson.
  --/ \version 1.24 Mark Dootson p[atched a problem with his previously submitted code
  --/               under high load from other processes. 
  --/ \version 1.25 Updated author and distribution location details to airspayce.com
  --/ \version 1.26 Added missing unmapmem for pads in bcm2835_close to prevent a memory leak. 
  --/               Reported by Hartmut Henkel.
  --/ \version 1.27 bcm2835_gpio_set_pad() no longer needs BCM2835_PAD_PASSWRD: it is
  --/               now automatically included.
  --/               Added suport for PWM mode with bcm2835_pwm_* functions.
  --/ \version 1.28 Fixed a problem where bcm2835_spi_writenb() would have problems with transfers of more than
  --/               64 bytes dues to read buffer filling. Patched by Peter Würtz.
  --/ \version 1.29 Further fix to SPI from Peter Würtz.
  --/ \version 1.30 10 microsecond delays from bcm2835_spi_transfer and bcm2835_spi_transfern for
  --/               significant performance improvements, Patch by Alan Watson.
  --/ \version 1.31 Fix a GCC warning about dummy variable, patched by Alan Watson. Thanks.
  --/ \version 1.32 Added option I2C_V1 definition to compile for version 1 RPi. 
  --/               By default I2C code is generated for the V2 RPi which has SDA1 and SCL1 connected.
  --/               Contributed by Malcolm Wiles based on work by Arvi Govindaraj.
  --/ \version 1.33 Added command line utilities i2c and gpio to examples. Contributed by Shahrooz Shahparnia.
  --/ \version 1.34 Added bcm2835_i2c_write_read_rs() which writes an arbitrary number of bytes, 
  --/               sends a repeat start, and reads from the device. Contributed by Eduardo Steinhorst.
  --/ \version 1.35 Fix build errors when compiled under Qt. Also performance improvements with SPI transfers. Contributed by Udo Klaas.
  --/ \version 1.36 Make automake's test runner detect that we're skipping tests when not root, the second
  --/               one makes us skip the test when using fakeroot (as used when building
  --/               Debian packages). Contributed by Guido Günther.
  --/
  --/ \author  Mike McCauley (mikem@airspayce.com) DO NOT CONTACT THE AUTHOR DIRECTLY: USE THE LISTS
  -- Defines for BCM2835
  --/ \defgroup constants Constants for passing to and from library functions
  --/ The values here are designed to be passed to various functions in the bcm2835 library.
  --/ @{
  --/ This means pin HIGH, true, 3.3volts on a pin.
  --/ This means pin LOW, false, 0volts on a pin.
  --/ Speed of the core clock core_clk
  -- Physical addresses for various peripheral register sets
  --/ Base Physical Address of the BCM 2835 peripheral registers
  --/ Base Physical Address of the System Timer registers
  --/ Base Physical Address of the Pads registers
  --/ Base Physical Address of the Clock/timer registers
  --/ Base Physical Address of the GPIO registers
  --/ Base Physical Address of the SPI0 registers
  --/ Base Physical Address of the BSC0 registers
  --/ Base Physical Address of the PWM registers
  --/ Base Physical Address of the BSC1 registers

  -- manual converted
  HIGH                            : constant unsigned := 16#1#;
  LOW                             : constant unsigned := 16#0#;
  BCM2835_CORE_CLK_HZ             : constant unsigned := 250000000;
  BCM2835_PERI_BASE               : constant unsigned := 16#20000000#;
  BCM2835_ST_BASE                 : constant unsigned := (BCM2835_PERI_BASE + 16#3000#);
  BCM2835_GPIO_PADS               : constant unsigned := (BCM2835_PERI_BASE + 16#100000#);
  BCM2835_CLOCK_BASE              : constant unsigned := (BCM2835_PERI_BASE + 16#101000#);
  BCM2835_GPIO_BASE               : constant unsigned := (BCM2835_PERI_BASE + 16#200000#);
  BCM2835_SPI0_BASE               : constant unsigned := (BCM2835_PERI_BASE + 16#204000#);
  BCM2835_BSC0_BASE               : constant unsigned := (BCM2835_PERI_BASE + 16#205000#);
  BCM2835_GPIO_PWM                : constant unsigned := (BCM2835_PERI_BASE + 16#20C000#);
  BCM2835_BSC1_BASE               : constant unsigned := (BCM2835_PERI_BASE + 16#804000#);
  BCM2835_PAGE_SIZE               : constant unsigned := (4*1024);
  BCM2835_BLOCK_SIZE              : constant unsigned := (4*1024);
  BCM2835_GPFSEL0                 : constant unsigned := 16#0000#;
  BCM2835_GPFSEL1                 : constant unsigned := 16#0004#;
  BCM2835_GPFSEL2                 : constant unsigned := 16#0008#;
  BCM2835_GPFSEL3                 : constant unsigned := 16#000c#;
  BCM2835_GPFSEL4                 : constant unsigned := 16#0010#;
  BCM2835_GPFSEL5                 : constant unsigned := 16#0014#;
  BCM2835_GPSET0                  : constant unsigned := 16#001c#;
  BCM2835_GPSET1                  : constant unsigned := 16#0020#;
  BCM2835_GPCLR0                  : constant unsigned := 16#0028#;
  BCM2835_GPCLR1                  : constant unsigned := 16#002c#;
  BCM2835_GPLEV0                  : constant unsigned := 16#0034#;
  BCM2835_GPLEV1                  : constant unsigned := 16#0038#;
  BCM2835_GPEDS0                  : constant unsigned := 16#0040#;
  BCM2835_GPEDS1                  : constant unsigned := 16#0044#;
  BCM2835_GPREN0                  : constant unsigned := 16#004c#;
  BCM2835_GPREN1                  : constant unsigned := 16#0050#;
  BCM2835_GPFEN0                  : constant unsigned := 16#0058#;
  BCM2835_GPFEN1                  : constant unsigned := 16#005c#;
  BCM2835_GPHEN0                  : constant unsigned := 16#0064#;
  BCM2835_GPHEN1                  : constant unsigned := 16#0068#;
  BCM2835_GPLEN0                  : constant unsigned := 16#0070#;
  BCM2835_GPLEN1                  : constant unsigned := 16#0074#;
  BCM2835_GPAREN0                 : constant unsigned := 16#007c#;
  BCM2835_GPAREN1                 : constant unsigned := 16#0080#;
  BCM2835_GPAFEN0                 : constant unsigned := 16#0088#;
  BCM2835_GPAFEN1                 : constant unsigned := 16#008c#;
  BCM2835_GPPUD                   : constant unsigned := 16#0094#;
  BCM2835_GPPUDCLK0               : constant unsigned := 16#0098#;
  BCM2835_GPPUDCLK1               : constant unsigned := 16#009c#;
  BCM2835_PADS_GPIO_0_27          : constant unsigned := 16#002c#;
  BCM2835_PADS_GPIO_28_45         : constant unsigned := 16#0030#;
  BCM2835_PADS_GPIO_46_53         : constant unsigned := 16#0034#;
  BCM2835_PAD_PASSWRD             : constant unsigned := 16#5A000000#;
  BCM2835_PAD_SLEW_RATE_UNLIMITED : constant unsigned := 16#10#;
  BCM2835_PAD_HYSTERESIS_ENABLED  : constant unsigned := 16#08#;
  BCM2835_PAD_DRIVE_2mA           : constant unsigned := 16#00#;
  BCM2835_PAD_DRIVE_4mA           : constant unsigned := 16#01#;
  BCM2835_PAD_DRIVE_6mA           : constant unsigned := 16#02#;
  BCM2835_PAD_DRIVE_8mA           : constant unsigned := 16#03#;
  BCM2835_PAD_DRIVE_10mA          : constant unsigned := 16#04#;
  BCM2835_PAD_DRIVE_12mA          : constant unsigned := 16#05#;
  BCM2835_PAD_DRIVE_14mA          : constant unsigned := 16#06#;
  BCM2835_PAD_DRIVE_16mA          : constant unsigned := 16#07#;
  BCM2835_SPI0_CS                 : constant unsigned := 16#0000#;
  BCM2835_SPI0_FIFO               : constant unsigned := 16#0004#;
  BCM2835_SPI0_CLK                : constant unsigned := 16#0008#;
  BCM2835_SPI0_DLEN               : constant unsigned := 16#000c#;
  BCM2835_SPI0_LTOH               : constant unsigned := 16#0010#;
  BCM2835_SPI0_DC                 : constant unsigned := 16#0014#;
  BCM2835_SPI0_CS_LEN_LONG        : constant unsigned := 16#02000000#;
  BCM2835_SPI0_CS_DMA_LEN         : constant unsigned := 16#01000000#;
  BCM2835_SPI0_CS_CSPOL2          : constant unsigned := 16#00800000#;
  BCM2835_SPI0_CS_CSPOL1          : constant unsigned := 16#00400000#;
  BCM2835_SPI0_CS_CSPOL0          : constant unsigned := 16#00200000#;
  BCM2835_SPI0_CS_RXF             : constant unsigned := 16#00100000#;
  BCM2835_SPI0_CS_RXR             : constant unsigned := 16#00080000#;
  BCM2835_SPI0_CS_TXD             : constant unsigned := 16#00040000#;
  BCM2835_SPI0_CS_RXD             : constant unsigned := 16#00020000#;
  BCM2835_SPI0_CS_DONE            : constant unsigned := 16#00010000#;
  BCM2835_SPI0_CS_TE_EN           : constant unsigned := 16#00008000#;
  BCM2835_SPI0_CS_LMONO           : constant unsigned := 16#00004000#;
  BCM2835_SPI0_CS_LEN             : constant unsigned := 16#00002000#;
  BCM2835_SPI0_CS_REN             : constant unsigned := 16#00001000#;
  BCM2835_SPI0_CS_ADCS            : constant unsigned := 16#00000800#;
  BCM2835_SPI0_CS_INTR            : constant unsigned := 16#00000400#;
  BCM2835_SPI0_CS_INTD            : constant unsigned := 16#00000200#;
  BCM2835_SPI0_CS_DMAEN           : constant unsigned := 16#00000100#;
  BCM2835_SPI0_CS_TA              : constant unsigned := 16#00000080#;
  BCM2835_SPI0_CS_CSPOL           : constant unsigned := 16#00000040#;
  BCM2835_SPI0_CS_CLEAR           : constant unsigned := 16#00000030#;
  BCM2835_SPI0_CS_CLEAR_RX        : constant unsigned := 16#00000020#;
  BCM2835_SPI0_CS_CLEAR_TX        : constant unsigned := 16#00000010#;
  BCM2835_SPI0_CS_CPOL            : constant unsigned := 16#00000008#;
  BCM2835_SPI0_CS_CPHA            : constant unsigned := 16#00000004#;
  BCM2835_SPI0_CS_CS              : constant unsigned := 16#00000003#;
  BCM2835_BSC_C                   : constant unsigned := 16#0000#;
  BCM2835_BSC_S                   : constant unsigned := 16#0004#;
  BCM2835_BSC_DLEN                : constant unsigned := 16#0008#;
  BCM2835_BSC_A                   : constant unsigned := 16#000c#;
  BCM2835_BSC_FIFO                : constant unsigned := 16#0010#;
  BCM2835_BSC_DIV                 : constant unsigned := 16#0014#;
  BCM2835_BSC_DEL                 : constant unsigned := 16#0018#;
  BCM2835_BSC_CLKT                : constant unsigned := 16#001c#;
  BCM2835_BSC_C_I2CEN             : constant unsigned := 16#00008000#;
  BCM2835_BSC_C_INTR              : constant unsigned := 16#00000400#;
  BCM2835_BSC_C_INTT              : constant unsigned := 16#00000200#;
  BCM2835_BSC_C_INTD              : constant unsigned := 16#00000100#;
  BCM2835_BSC_C_ST                : constant unsigned := 16#00000080#;
  BCM2835_BSC_C_CLEAR_1           : constant unsigned := 16#00000020#;
  BCM2835_BSC_C_CLEAR_2           : constant unsigned := 16#00000010#;
  BCM2835_BSC_C_READ              : constant unsigned := 16#00000001#;
  BCM2835_BSC_S_CLKT              : constant unsigned := 16#00000200#;
  BCM2835_BSC_S_ERR               : constant unsigned := 16#00000100#;
  BCM2835_BSC_S_RXF               : constant unsigned := 16#00000080#;
  BCM2835_BSC_S_TXE               : constant unsigned := 16#00000040#;
  BCM2835_BSC_S_RXD               : constant unsigned := 16#00000020#;
  BCM2835_BSC_S_TXD               : constant unsigned := 16#00000010#;
  BCM2835_BSC_S_RXR               : constant unsigned := 16#00000008#;
  BCM2835_BSC_S_TXW               : constant unsigned := 16#00000004#;
  BCM2835_BSC_S_DONE              : constant unsigned := 16#00000002#;
  BCM2835_BSC_S_TA                : constant unsigned := 16#00000001#;
  BCM2835_BSC_FIFO_SIZE           : constant unsigned := 16;
  BCM2835_ST_CS                   : constant unsigned := 16#0000#;
  BCM2835_ST_CLO                  : constant unsigned := 16#0004#;
  BCM2835_ST_CHI                  : constant unsigned := 16#0008#;
  BCM2835_PWM_CONTROL             : constant unsigned := 0;
  BCM2835_PWM_STATUS              : constant unsigned := 1;
  BCM2835_PWM_DMAC                : constant unsigned := 2;
  BCM2835_PWM0_RANGE              : constant unsigned := 4;
  BCM2835_PWM0_DATA               : constant unsigned := 5;
  BCM2835_PWM_FIF1                : constant unsigned := 6;
  BCM2835_PWM1_RANGE              : constant unsigned := 8;
  BCM2835_PWM1_DATA               : constant unsigned := 9;
  BCM2835_PWMCLK_CNTL             : constant unsigned := 40;
  BCM2835_PWMCLK_DIV              : constant unsigned := 41;
  BCM2835_PWM_PASSWRD             : constant unsigned := 16#5A000000#;
  BCM2835_PWM1_MS_MODE            : constant unsigned := 16#8000#;
  BCM2835_PWM1_USEFIFO            : constant unsigned := 16#2000#;
  BCM2835_PWM1_REVPOLAR           : constant unsigned := 16#1000#;
  BCM2835_PWM1_OFFSTATE           : constant unsigned := 16#0800#;
  BCM2835_PWM1_REPEATFF           : constant unsigned := 16#0400#;
  BCM2835_PWM1_SERIAL             : constant unsigned := 16#0200#;
  BCM2835_PWM1_ENABLE             : constant unsigned := 16#0100#;
  BCM2835_PWM0_MS_MODE            : constant unsigned := 16#0080#;
  BCM2835_PWM_CLEAR_FIFO          : constant unsigned := 16#0040#;
  BCM2835_PWM0_USEFIFO            : constant unsigned := 16#0020#;
  BCM2835_PWM0_REVPOLAR           : constant unsigned := 16#0010#;
  BCM2835_PWM0_OFFSTATE           : constant unsigned := 16#0008#;
  BCM2835_PWM0_REPEATFF           : constant unsigned := 16#0004#;
  BCM2835_PWM0_SERIAL             : constant unsigned := 16#0002#;
  BCM2835_PWM0_ENABLE             : constant unsigned := 16#0001#;
   --  arg-macro: procedure delay (x)
   --    bcm2835_delay(x)
   --  arg-macro: procedure delayMicroseconds (x)
   --    bcm2835_delayMicroseconds(x)


  --/ Base of the ST (System Timer) registers.
  --/ Available after bcm2835_init has been called
   bcm2835_st : access stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:355
   pragma Import (C, bcm2835_st, "bcm2835_st");

  --/ Base of the GPIO registers.
  --/ Available after bcm2835_init has been called
   bcm2835_gpio : access stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:359
   pragma Import (C, bcm2835_gpio, "bcm2835_gpio");

  --/ Base of the PWM registers.
  --/ Available after bcm2835_init has been called
   bcm2835_pwm : access stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:363
   pragma Import (C, bcm2835_pwm, "bcm2835_pwm");

  --/ Base of the CLK registers.
  --/ Available after bcm2835_init has been called
   bcm2835_clk : access stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:367
   pragma Import (C, bcm2835_clk, "bcm2835_clk");

  --/ Base of the PADS registers.
  --/ Available after bcm2835_init has been called
   bcm2835_pads : access stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:371
   pragma Import (C, bcm2835_pads, "bcm2835_pads");

  --/ Base of the SPI0 registers.
  --/ Available after bcm2835_init has been called
   bcm2835_spi0 : access stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:375
   pragma Import (C, bcm2835_spi0, "bcm2835_spi0");

  --/ Base of the BSC0 registers.
  --/ Available after bcm2835_init has been called
   bcm2835_bsc0 : access stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:379
   pragma Import (C, bcm2835_bsc0, "bcm2835_bsc0");

  --/ Base of the BSC1 registers.
  --/ Available after bcm2835_init has been called
   bcm2835_bsc1 : access stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:383
   pragma Import (C, bcm2835_bsc1, "bcm2835_bsc1");

  --/ Size of memory page on RPi
  --/ Size of memory block on RPi
  -- Defines for GPIO
  -- The BCM2835 has 54 GPIO pins.
  --      BCM2835 data sheet, Page 90 onwards.
  --/ GPIO register offsets from BCM2835_GPIO_BASE. Offsets into the GPIO Peripheral block in bytes per 6.1 Register View
  --/ \brief bcm2835PortFunction
  --/ Port function select modes for bcm2835_gpio_fsel()
  --/< Input
  --/< Output
  --/< Alternate function 0
  --/< Alternate function 1
  --/< Alternate function 2
  --/< Alternate function 3
  --/< Alternate function 4
  --/< Alternate function 5
  --/< Function select bits mask
   subtype bcm2835FunctionSelect is unsigned;
   BCM2835_GPIO_FSEL_INPT : constant bcm2835FunctionSelect := 0;
   BCM2835_GPIO_FSEL_OUTP : constant bcm2835FunctionSelect := 1;
   BCM2835_GPIO_FSEL_ALT0 : constant bcm2835FunctionSelect := 4;
   BCM2835_GPIO_FSEL_ALT1 : constant bcm2835FunctionSelect := 5;
   BCM2835_GPIO_FSEL_ALT2 : constant bcm2835FunctionSelect := 6;
   BCM2835_GPIO_FSEL_ALT3 : constant bcm2835FunctionSelect := 7;
   BCM2835_GPIO_FSEL_ALT4 : constant bcm2835FunctionSelect := 3;
   BCM2835_GPIO_FSEL_ALT5 : constant bcm2835FunctionSelect := 2;
   BCM2835_GPIO_FSEL_MASK : constant bcm2835FunctionSelect := 7;  -- /usr/local/include/bcm2835.h:438

  --/ \brief bcm2835PUDControl
  --/ Pullup/Pulldown defines for bcm2835_gpio_pud()
  --/< Off ? disable pull-up/down
  --/< Enable Pull Down control
  --/< Enable Pull Up control
   type bcm2835PUDControl is 
     (BCM2835_GPIO_PUD_OFF,
      BCM2835_GPIO_PUD_DOWN,
      BCM2835_GPIO_PUD_UP);
   pragma Convention (C, bcm2835PUDControl);  -- /usr/local/include/bcm2835.h:447

  --/ Pad control register offsets from BCM2835_GPIO_PADS
  --/ Pad Control masks
  --/ \brief bcm2835PadGroup
  --/ Pad group specification for bcm2835_gpio_pad()
  --/< Pad group for GPIO pads 0 to 27
  --/< Pad group for GPIO pads 28 to 45
  --/< Pad group for GPIO pads 46 to 53
   type bcm2835PadGroup is 
     (BCM2835_PAD_GROUP_GPIO_0_27,
      BCM2835_PAD_GROUP_GPIO_28_45,
      BCM2835_PAD_GROUP_GPIO_46_53);
   pragma Convention (C, bcm2835PadGroup);  -- /usr/local/include/bcm2835.h:474

  --/ \brief GPIO Pin Numbers
  --/
  --/ Here we define Raspberry Pin GPIO pins on P1 in terms of the underlying BCM GPIO pin numbers.
  --/ These can be passed as a pin number to any function requiring a pin.
  --/ Not all pins on the RPi 26 bin IDE plug are connected to GPIO pins
  --/ and some can adopt an alternate function.
  --/ RPi version 2 has some slightly different pinouts, and these are values RPI_V2_*.
  --/ At bootup, pins 8 and 10 are set to UART0_TXD, UART0_RXD (ie the alt0 function) respectively
  --/ When SPI0 is in use (ie after bcm2835_spi_begin()), pins 19, 21, 23, 24, 26 are dedicated to SPI
  --/ and cant be controlled independently
  --/< Version 1, Pin P1-03
  --/< Version 1, Pin P1-05
  --/< Version 1, Pin P1-07
  --/< Version 1, Pin P1-08, defaults to alt function 0 UART0_TXD
  --/< Version 1, Pin P1-10, defaults to alt function 0 UART0_RXD
  --/< Version 1, Pin P1-11
  --/< Version 1, Pin P1-12, can be PWM channel 0 in ALT FUN 5
  --/< Version 1, Pin P1-13
  --/< Version 1, Pin P1-15
  --/< Version 1, Pin P1-16
  --/< Version 1, Pin P1-18
  --/< Version 1, Pin P1-19, MOSI when SPI0 in use
  --/< Version 1, Pin P1-21, MISO when SPI0 in use
  --/< Version 1, Pin P1-22
  --/< Version 1, Pin P1-23, CLK when SPI0 in use
  --/< Version 1, Pin P1-24, CE0 when SPI0 in use
  --/< Version 1, Pin P1-26, CE1 when SPI0 in use
  -- RPi Version 2
  --/< Version 2, Pin P1-03
  --/< Version 2, Pin P1-05
  --/< Version 2, Pin P1-07
  --/< Version 2, Pin P1-08, defaults to alt function 0 UART0_TXD
  --/< Version 2, Pin P1-10, defaults to alt function 0 UART0_RXD
  --/< Version 2, Pin P1-11
  --/< Version 2, Pin P1-12, can be PWM channel 0 in ALT FUN 5
  --/< Version 2, Pin P1-13
  --/< Version 2, Pin P1-15
  --/< Version 2, Pin P1-16
  --/< Version 2, Pin P1-18
  --/< Version 2, Pin P1-19, MOSI when SPI0 in use
  --/< Version 2, Pin P1-21, MISO when SPI0 in use
  --/< Version 2, Pin P1-22
  --/< Version 2, Pin P1-23, CLK when SPI0 in use
  --/< Version 2, Pin P1-24, CE0 when SPI0 in use
  --/< Version 2, Pin P1-26, CE1 when SPI0 in use
  -- RPi Version 2, new plug P5
  --/< Version 2, Pin P5-03
  --/< Version 2, Pin P5-04
  --/< Version 2, Pin P5-05
  --/< Version 2, Pin P5-06
   subtype RPiGPIOPin is unsigned;
   RPI_GPIO_P1_03 : constant RPiGPIOPin := 0;
   RPI_GPIO_P1_05 : constant RPiGPIOPin := 1;
   RPI_GPIO_P1_07 : constant RPiGPIOPin := 4;
   RPI_GPIO_P1_08 : constant RPiGPIOPin := 14;
   RPI_GPIO_P1_10 : constant RPiGPIOPin := 15;
   RPI_GPIO_P1_11 : constant RPiGPIOPin := 17;
   RPI_GPIO_P1_12 : constant RPiGPIOPin := 18;
   RPI_GPIO_P1_13 : constant RPiGPIOPin := 21;
   RPI_GPIO_P1_15 : constant RPiGPIOPin := 22;
   RPI_GPIO_P1_16 : constant RPiGPIOPin := 23;
   RPI_GPIO_P1_18 : constant RPiGPIOPin := 24;
   RPI_GPIO_P1_19 : constant RPiGPIOPin := 10;
   RPI_GPIO_P1_21 : constant RPiGPIOPin := 9;
   RPI_GPIO_P1_22 : constant RPiGPIOPin := 25;
   RPI_GPIO_P1_23 : constant RPiGPIOPin := 11;
   RPI_GPIO_P1_24 : constant RPiGPIOPin := 8;
   RPI_GPIO_P1_26 : constant RPiGPIOPin := 7;
   RPI_V2_GPIO_P1_03 : constant RPiGPIOPin := 2;
   RPI_V2_GPIO_P1_05 : constant RPiGPIOPin := 3;
   RPI_V2_GPIO_P1_07 : constant RPiGPIOPin := 4;
   RPI_V2_GPIO_P1_08 : constant RPiGPIOPin := 14;
   RPI_V2_GPIO_P1_10 : constant RPiGPIOPin := 15;
   RPI_V2_GPIO_P1_11 : constant RPiGPIOPin := 17;
   RPI_V2_GPIO_P1_12 : constant RPiGPIOPin := 18;
   RPI_V2_GPIO_P1_13 : constant RPiGPIOPin := 27;
   RPI_V2_GPIO_P1_15 : constant RPiGPIOPin := 22;
   RPI_V2_GPIO_P1_16 : constant RPiGPIOPin := 23;
   RPI_V2_GPIO_P1_18 : constant RPiGPIOPin := 24;
   RPI_V2_GPIO_P1_19 : constant RPiGPIOPin := 10;
   RPI_V2_GPIO_P1_21 : constant RPiGPIOPin := 9;
   RPI_V2_GPIO_P1_22 : constant RPiGPIOPin := 25;
   RPI_V2_GPIO_P1_23 : constant RPiGPIOPin := 11;
   RPI_V2_GPIO_P1_24 : constant RPiGPIOPin := 8;
   RPI_V2_GPIO_P1_26 : constant RPiGPIOPin := 7;
   RPI_V2_GPIO_P5_03 : constant RPiGPIOPin := 28;
   RPI_V2_GPIO_P5_04 : constant RPiGPIOPin := 29;
   RPI_V2_GPIO_P5_05 : constant RPiGPIOPin := 30;
   RPI_V2_GPIO_P5_06 : constant RPiGPIOPin := 31;  -- /usr/local/include/bcm2835.h:531

  -- Defines for SPI
  -- GPIO register offsets from BCM2835_SPI0_BASE. 
  -- Offsets into the SPI Peripheral block in bytes per 10.5 SPI Register Map
  -- Register masks for SPI0_CS
  --/ \brief bcm2835SPIBitOrder SPI Bit order
  --/ Specifies the SPI data bit ordering for bcm2835_spi_setBitOrder()
  --/< LSB First
  --/< MSB First
   type bcm2835SPIBitOrder is 
     (BCM2835_SPI_BIT_ORDER_LSBFIRST,
      BCM2835_SPI_BIT_ORDER_MSBFIRST);
   pragma Convention (C, bcm2835SPIBitOrder);  -- /usr/local/include/bcm2835.h:577

  --/ \brief SPI Data mode
  --/ Specify the SPI data mode to be passed to bcm2835_spi_setDataMode()
  --/< CPOL = 0, CPHA = 0
  --/< CPOL = 0, CPHA = 1
  --/< CPOL = 1, CPHA = 0
  --/< CPOL = 1, CPHA = 1
   type bcm2835SPIMode is 
     (BCM2835_SPI_MODE0,
      BCM2835_SPI_MODE1,
      BCM2835_SPI_MODE2,
      BCM2835_SPI_MODE3);
   pragma Convention (C, bcm2835SPIMode);  -- /usr/local/include/bcm2835.h:587

  --/ \brief bcm2835SPIChipSelect
  --/ Specify the SPI chip select pin(s)
  --/< Chip Select 0
  --/< Chip Select 1
  --/< Chip Select 2 (ie pins CS1 and CS2 are asserted)
  --/< No CS, control it yourself
   type bcm2835SPIChipSelect is 
     (BCM2835_SPI_CS0,
      BCM2835_SPI_CS1,
      BCM2835_SPI_CS2,
      BCM2835_SPI_CS_NONE);
   pragma Convention (C, bcm2835SPIChipSelect);  -- /usr/local/include/bcm2835.h:597

  --/ \brief bcm2835SPIClockDivider
  --/ Specifies the divider used to generate the SPI clock from the system clock.
  --/ Figures below give the divider, clock period and clock frequency.
  --/ Clock divided is based on nominal base clock rate of 250MHz
  --/ It is reported that (contrary to the documentation) any even divider may used.
  --/ The frequencies shown for each divider have been confirmed by measurement
  --/< 65536 = 262.144us = 3.814697260kHz
  --/< 32768 = 131.072us = 7.629394531kHz
  --/< 16384 = 65.536us = 15.25878906kHz
  --/< 8192 = 32.768us = 30/51757813kHz
  --/< 4096 = 16.384us = 61.03515625kHz
  --/< 2048 = 8.192us = 122.0703125kHz
  --/< 1024 = 4.096us = 244.140625kHz
  --/< 512 = 2.048us = 488.28125kHz
  --/< 256 = 1.024us = 976.5625MHz
  --/< 128 = 512ns = = 1.953125MHz
  --/< 64 = 256ns = 3.90625MHz
  --/< 32 = 128ns = 7.8125MHz
  --/< 16 = 64ns = 15.625MHz
  --/< 8 = 32ns = 31.25MHz
  --/< 4 = 16ns = 62.5MHz
  --/< 2 = 8ns = 125MHz, fastest you can get
  --/< 1 = 262.144us = 3.814697260kHz, same as 0/65536
   subtype bcm2835SPIClockDivider is unsigned;
   BCM2835_SPI_CLOCK_DIVIDER_65536 : constant bcm2835SPIClockDivider := 0;
   BCM2835_SPI_CLOCK_DIVIDER_32768 : constant bcm2835SPIClockDivider := 32768;
   BCM2835_SPI_CLOCK_DIVIDER_16384 : constant bcm2835SPIClockDivider := 16384;
   BCM2835_SPI_CLOCK_DIVIDER_8192 : constant bcm2835SPIClockDivider := 8192;
   BCM2835_SPI_CLOCK_DIVIDER_4096 : constant bcm2835SPIClockDivider := 4096;
   BCM2835_SPI_CLOCK_DIVIDER_2048 : constant bcm2835SPIClockDivider := 2048;
   BCM2835_SPI_CLOCK_DIVIDER_1024 : constant bcm2835SPIClockDivider := 1024;
   BCM2835_SPI_CLOCK_DIVIDER_512 : constant bcm2835SPIClockDivider := 512;
   BCM2835_SPI_CLOCK_DIVIDER_256 : constant bcm2835SPIClockDivider := 256;
   BCM2835_SPI_CLOCK_DIVIDER_128 : constant bcm2835SPIClockDivider := 128;
   BCM2835_SPI_CLOCK_DIVIDER_64 : constant bcm2835SPIClockDivider := 64;
   BCM2835_SPI_CLOCK_DIVIDER_32 : constant bcm2835SPIClockDivider := 32;
   BCM2835_SPI_CLOCK_DIVIDER_16 : constant bcm2835SPIClockDivider := 16;
   BCM2835_SPI_CLOCK_DIVIDER_8 : constant bcm2835SPIClockDivider := 8;
   BCM2835_SPI_CLOCK_DIVIDER_4 : constant bcm2835SPIClockDivider := 4;
   BCM2835_SPI_CLOCK_DIVIDER_2 : constant bcm2835SPIClockDivider := 2;
   BCM2835_SPI_CLOCK_DIVIDER_1 : constant bcm2835SPIClockDivider := 1;  -- /usr/local/include/bcm2835.h:624

  -- Defines for I2C
  -- GPIO register offsets from BCM2835_BSC*_BASE.
  -- Offsets into the BSC Peripheral block in bytes per 3.1 BSC Register Map
  -- Register masks for BSC_C
  -- Register masks for BSC_S
  --/ \brief bcm2835I2CClockDivider
  --/ Specifies the divider used to generate the I2C clock from the system clock.
  --/ Clock divided is based on nominal base clock rate of 250MHz
  --/< 2500 = 10us = 100 kHz
  --/< 622 = 2.504us = 399.3610 kHz
  --/< 150 = 60ns = 1.666 MHz (default at reset)
  --/< 148 = 59ns = 1.689 MHz
   subtype bcm2835I2CClockDivider is unsigned;
   BCM2835_I2C_CLOCK_DIVIDER_2500 : constant bcm2835I2CClockDivider := 2500;
   BCM2835_I2C_CLOCK_DIVIDER_626 : constant bcm2835I2CClockDivider := 626;
   BCM2835_I2C_CLOCK_DIVIDER_150 : constant bcm2835I2CClockDivider := 150;
   BCM2835_I2C_CLOCK_DIVIDER_148 : constant bcm2835I2CClockDivider := 148;  -- /usr/local/include/bcm2835.h:671

  --/ \brief bcm2835I2CReasonCodes
  --/ Specifies the reason codes for the bcm2835_i2c_write and bcm2835_i2c_read functions.
  --/< Success
  --/< Received a NACK
  --/< Received Clock Stretch Timeout
  --/< Not all data is sent / received
   subtype bcm2835I2CReasonCodes is unsigned;
   BCM2835_I2C_REASON_OK : constant bcm2835I2CReasonCodes := 0;
   BCM2835_I2C_REASON_ERROR_NACK : constant bcm2835I2CReasonCodes := 1;
   BCM2835_I2C_REASON_ERROR_CLKT : constant bcm2835I2CReasonCodes := 2;
   BCM2835_I2C_REASON_ERROR_DATA : constant bcm2835I2CReasonCodes := 4;  -- /usr/local/include/bcm2835.h:681

  -- Defines for ST
  -- GPIO register offsets from BCM2835_ST_BASE.
  -- Offsets into the ST Peripheral block in bytes per 12.1 System Timer Registers
  -- The System Timer peripheral provides four 32-bit timer channels and a single 64-bit free running counter.
  -- BCM2835_ST_CLO is the System Timer Counter Lower bits register.
  -- The system timer free-running counter lower register is a read-only register that returns the current value
  -- of the lower 32-bits of the free running counter.
  -- BCM2835_ST_CHI is the System Timer Counter Upper bits register.
  -- The system timer free-running counter upper register is a read-only register that returns the current value
  -- of the upper 32-bits of the free running counter.
  --/ @}
  -- Defines for PWM, word offsets (ie 4 byte multiples)
  -- Defines for PWM Clock, word offsets (ie 4 byte multiples)
  --/ \brief bcm2835PWMClockDivider
  --/ Specifies the divider used to generate the PWM clock from the system clock.
  --/ Figures below give the divider, clock period and clock frequency.
  --/ Clock divided is based on nominal PWM base clock rate of 19.2MHz
  --/ The frequencies shown for each divider have been confirmed by measurement
  --/< 32768 = 585Hz
  --/< 16384 = 1171.8Hz
  --/< 8192 = 2.34375kHz
  --/< 4096 = 4.6875kHz
  --/< 2048 = 9.375kHz
  --/< 1024 = 18.75kHz
  --/< 512 = 37.5kHz
  --/< 256 = 75kHz
  --/< 128 = 150kHz
  --/< 64 = 300kHz
  --/< 32 = 600.0kHz
  --/< 16 = 1.2MHz
  --/< 8 = 2.4MHz
  --/< 4 = 4.8MHz
  --/< 2 = 9.6MHz, fastest you can get
  --/< 1 = 4.6875kHz, same as divider 4096
   subtype bcm2835PWMClockDivider is unsigned;
   BCM2835_PWM_CLOCK_DIVIDER_32768 : constant bcm2835PWMClockDivider := 32768;
   BCM2835_PWM_CLOCK_DIVIDER_16384 : constant bcm2835PWMClockDivider := 16384;
   BCM2835_PWM_CLOCK_DIVIDER_8192 : constant bcm2835PWMClockDivider := 8192;
   BCM2835_PWM_CLOCK_DIVIDER_4096 : constant bcm2835PWMClockDivider := 4096;
   BCM2835_PWM_CLOCK_DIVIDER_2048 : constant bcm2835PWMClockDivider := 2048;
   BCM2835_PWM_CLOCK_DIVIDER_1024 : constant bcm2835PWMClockDivider := 1024;
   BCM2835_PWM_CLOCK_DIVIDER_512 : constant bcm2835PWMClockDivider := 512;
   BCM2835_PWM_CLOCK_DIVIDER_256 : constant bcm2835PWMClockDivider := 256;
   BCM2835_PWM_CLOCK_DIVIDER_128 : constant bcm2835PWMClockDivider := 128;
   BCM2835_PWM_CLOCK_DIVIDER_64 : constant bcm2835PWMClockDivider := 64;
   BCM2835_PWM_CLOCK_DIVIDER_32 : constant bcm2835PWMClockDivider := 32;
   BCM2835_PWM_CLOCK_DIVIDER_16 : constant bcm2835PWMClockDivider := 16;
   BCM2835_PWM_CLOCK_DIVIDER_8 : constant bcm2835PWMClockDivider := 8;
   BCM2835_PWM_CLOCK_DIVIDER_4 : constant bcm2835PWMClockDivider := 4;
   BCM2835_PWM_CLOCK_DIVIDER_2 : constant bcm2835PWMClockDivider := 2;
   BCM2835_PWM_CLOCK_DIVIDER_1 : constant bcm2835PWMClockDivider := 1;  -- /usr/local/include/bcm2835.h:755

  -- Historical name compatibility
  --/ \defgroup init Library initialisation and management
  --/ These functions allow you to intialise and control the bcm2835 library
  --/ @{
  --/ Initialise the library by opening /dev/mem and getting pointers to the 
  --/ internal memory for BCM 2835 device registers. You must call this (successfully)
  --/ before calling any other 
  --/ functions in this library (except bcm2835_set_debug). 
  --/ If bcm2835_init() fails by returning 0, 
  --/ calling any other function may result in crashes or other failures.
  --/ Prints messages to stderr in case of errors.
  --/ \return 1 if successful else 0
   function bcm2835_init return int;  -- /usr/local/include/bcm2835.h:779
   pragma Import (C, bcm2835_init, "bcm2835_init");

  --/ Close the library, deallocating any allocated memory and closing /dev/mem
  --/ \return 1 if successful else 0
   function bcm2835_close return int;  -- /usr/local/include/bcm2835.h:783
   pragma Import (C, bcm2835_close, "bcm2835_close");

  --/ Sets the debug level of the library.
  --/ A value of 1 prevents mapping to /dev/mem, and makes the library print out
  --/ what it would do, rather than accessing the GPIO registers.
  --/ A value of 0, the default, causes normal operation.
  --/ Call this before calling bcm2835_init();
  --/ \param[in] debug The new debug level. 1 means debug
   procedure bcm2835_set_debug (debug : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:791
   pragma Import (C, bcm2835_set_debug, "bcm2835_set_debug");

  --/ @} // end of init
  --/ \defgroup lowlevel Low level register access
  --/ These functions provide low level register access, and should not generally
  --/ need to be used 
  --/ 
  --/ @{
  --/ Reads 32 bit value from a peripheral address
  --/ The read is done twice, and is therefore always safe in terms of 
  --/ manual section 1.3 Peripheral access precautions for correct memory ordering
  --/ \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
  --/ \return the value read from the 32 bit register
  --/ \sa Physical Addresses
   function bcm2835_peri_read (paddr : access stdint_h.uint32_t) return stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:807
   pragma Import (C, bcm2835_peri_read, "bcm2835_peri_read");

  --/ Reads 32 bit value from a peripheral address without the read barrier
  --/ You should only use this when your code has previously called bcm2835_peri_read()
  --/ within the same peripheral, and no other peripheral access has occurred since.
  --/ \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
  --/ \return the value read from the 32 bit register
  --/ \sa Physical Addresses
   function bcm2835_peri_read_nb (paddr : access stdint_h.uint32_t) return stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:816
   pragma Import (C, bcm2835_peri_read_nb, "bcm2835_peri_read_nb");

  --/ Writes 32 bit value from a peripheral address
  --/ The write is done twice, and is therefore always safe in terms of 
  --/ manual section 1.3 Peripheral access precautions for correct memory ordering
  --/ \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
  --/ \param[in] value The 32 bit value to write
  --/ \sa Physical Addresses
   procedure bcm2835_peri_write (paddr : access stdint_h.uint32_t; value : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:825
   pragma Import (C, bcm2835_peri_write, "bcm2835_peri_write");

  --/ Writes 32 bit value from a peripheral address without the write barrier
  --/ You should only use this when your code has previously called bcm2835_peri_write()
  --/ within the same peripheral, and no other peripheral access has occurred since.
  --/ \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
  --/ \param[in] value The 32 bit value to write
  --/ \sa Physical Addresses
   procedure bcm2835_peri_write_nb (paddr : access stdint_h.uint32_t; value : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:833
   pragma Import (C, bcm2835_peri_write_nb, "bcm2835_peri_write_nb");

  --/ Alters a number of bits in a 32 peripheral regsiter.
  --/ It reads the current valu and then alters the bits deines as 1 in mask, 
  --/ according to the bit value in value. 
  --/ All other bits that are 0 in the mask are unaffected.
  --/ Use this to alter a subset of the bits in a register.
  --/ The write is done twice, and is therefore always safe in terms of 
  --/ manual section 1.3 Peripheral access precautions for correct memory ordering
  --/ \param[in] paddr Physical address to read from. See BCM2835_GPIO_BASE etc.
  --/ \param[in] value The 32 bit value to write, masked in by mask.
  --/ \param[in] mask Bitmask that defines the bits that will be altered in the register.
  --/ \sa Physical Addresses
   procedure bcm2835_peri_set_bits
     (paddr : access stdint_h.uint32_t;
      value : stdint_h.uint32_t;
      mask : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:846
   pragma Import (C, bcm2835_peri_set_bits, "bcm2835_peri_set_bits");

  --/ @} // end of lowlevel
  --/ \defgroup gpio GPIO register access
  --/ These functions allow you to control the GPIO interface. You can set the 
  --/ function of each GPIO pin, read the input state and set the output state.
  --/ @{
  --/ Sets the Function Select register for the given pin, which configures
  --/ the pin as Input, Output or one of the 6 alternate functions.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  --/ \param[in] mode Mode to set the pin to, one of BCM2835_GPIO_FSEL_* from \ref bcm2835FunctionSelect
   procedure bcm2835_gpio_fsel (pin : stdint_h.uint8_t; mode : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:858
   pragma Import (C, bcm2835_gpio_fsel, "bcm2835_gpio_fsel");

  --/ Sets the specified pin output to 
  --/ HIGH.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  --/ \sa bcm2835_gpio_write()
   procedure bcm2835_gpio_set (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:864
   pragma Import (C, bcm2835_gpio_set, "bcm2835_gpio_set");

  --/ Sets the specified pin output to 
  --/ LOW.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  --/ \sa bcm2835_gpio_write()
   procedure bcm2835_gpio_clr (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:870
   pragma Import (C, bcm2835_gpio_clr, "bcm2835_gpio_clr");

  --/ Sets any of the first 32 GPIO output pins specified in the mask to 
  --/ HIGH.
  --/ \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
  --/ \sa bcm2835_gpio_write_multi()
   procedure bcm2835_gpio_set_multi (mask : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:876
   pragma Import (C, bcm2835_gpio_set_multi, "bcm2835_gpio_set_multi");

  --/ Sets any of the first 32 GPIO output pins specified in the mask to 
  --/ LOW.
  --/ \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
  --/ \sa bcm2835_gpio_write_multi()
   procedure bcm2835_gpio_clr_multi (mask : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:882
   pragma Import (C, bcm2835_gpio_clr_multi, "bcm2835_gpio_clr_multi");

  --/ Reads the current level on the specified 
  --/ pin and returns either HIGH or LOW. Works whether or not the pin
  --/ is an input or an output.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  --/ \return the current level  either HIGH or LOW
   function bcm2835_gpio_lev (pin : stdint_h.uint8_t) return stdint_h.uint8_t;  -- /usr/local/include/bcm2835.h:889
   pragma Import (C, bcm2835_gpio_lev, "bcm2835_gpio_lev");

  --/ Event Detect Status.
  --/ Tests whether the specified pin has detected a level or edge
  --/ as requested by bcm2835_gpio_ren(), bcm2835_gpio_fen(), bcm2835_gpio_hen(), 
  --/ bcm2835_gpio_len(), bcm2835_gpio_aren(), bcm2835_gpio_afen().
  --/ Clear the flag for a given pin by calling bcm2835_gpio_set_eds(pin);
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  --/ \return HIGH if the event detect status for the given pin is true.
   function bcm2835_gpio_eds (pin : stdint_h.uint8_t) return stdint_h.uint8_t;  -- /usr/local/include/bcm2835.h:898
   pragma Import (C, bcm2835_gpio_eds, "bcm2835_gpio_eds");

  --/ Sets the Event Detect Status register for a given pin to 1, 
  --/ which has the effect of clearing the flag. Use this afer seeing
  --/ an Event Detect Status on the pin.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_set_eds (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:904
   pragma Import (C, bcm2835_gpio_set_eds, "bcm2835_gpio_set_eds");

  --/ Enable Rising Edge Detect Enable for the specified pin.
  --/ When a rising edge is detected, sets the appropriate pin in Event Detect Status.
  --/ The GPRENn registers use
  --/ synchronous edge detection. This means the input signal is sampled using the
  --/ system clock and then it is looking for a ?011? pattern on the sampled signal. This
  --/ has the effect of suppressing glitches.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_ren (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:913
   pragma Import (C, bcm2835_gpio_ren, "bcm2835_gpio_ren");

  --/ Disable Rising Edge Detect Enable for the specified pin.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_clr_ren (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:917
   pragma Import (C, bcm2835_gpio_clr_ren, "bcm2835_gpio_clr_ren");

  --/ Enable Falling Edge Detect Enable for the specified pin.
  --/ When a falling edge is detected, sets the appropriate pin in Event Detect Status.
  --/ The GPRENn registers use
  --/ synchronous edge detection. This means the input signal is sampled using the
  --/ system clock and then it is looking for a ?100? pattern on the sampled signal. This
  --/ has the effect of suppressing glitches.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_fen (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:926
   pragma Import (C, bcm2835_gpio_fen, "bcm2835_gpio_fen");

  --/ Disable Falling Edge Detect Enable for the specified pin.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_clr_fen (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:930
   pragma Import (C, bcm2835_gpio_clr_fen, "bcm2835_gpio_clr_fen");

  --/ Enable High Detect Enable for the specified pin.
  --/ When a HIGH level is detected on the pin, sets the appropriate pin in Event Detect Status.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_hen (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:935
   pragma Import (C, bcm2835_gpio_hen, "bcm2835_gpio_hen");

  --/ Disable High Detect Enable for the specified pin.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_clr_hen (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:939
   pragma Import (C, bcm2835_gpio_clr_hen, "bcm2835_gpio_clr_hen");

  --/ Enable Low Detect Enable for the specified pin.
  --/ When a LOW level is detected on the pin, sets the appropriate pin in Event Detect Status.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_len (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:944
   pragma Import (C, bcm2835_gpio_len, "bcm2835_gpio_len");

  --/ Disable Low Detect Enable for the specified pin.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_clr_len (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:948
   pragma Import (C, bcm2835_gpio_clr_len, "bcm2835_gpio_clr_len");

  --/ Enable Asynchronous Rising Edge Detect Enable for the specified pin.
  --/ When a rising edge is detected, sets the appropriate pin in Event Detect Status.
  --/ Asynchronous means the incoming signal is not sampled by the system clock. As such
  --/ rising edges of very short duration can be detected.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_aren (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:955
   pragma Import (C, bcm2835_gpio_aren, "bcm2835_gpio_aren");

  --/ Disable Asynchronous Rising Edge Detect Enable for the specified pin.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_clr_aren (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:959
   pragma Import (C, bcm2835_gpio_clr_aren, "bcm2835_gpio_clr_aren");

  --/ Enable Asynchronous Falling Edge Detect Enable for the specified pin.
  --/ When a falling edge is detected, sets the appropriate pin in Event Detect Status.
  --/ Asynchronous means the incoming signal is not sampled by the system clock. As such
  --/ falling edges of very short duration can be detected.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_afen (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:966
   pragma Import (C, bcm2835_gpio_afen, "bcm2835_gpio_afen");

  --/ Disable Asynchronous Falling Edge Detect Enable for the specified pin.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
   procedure bcm2835_gpio_clr_afen (pin : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:970
   pragma Import (C, bcm2835_gpio_clr_afen, "bcm2835_gpio_clr_afen");

  --/ Sets the Pull-up/down register for the given pin. This is
  --/ used with bcm2835_gpio_pudclk() to set the  Pull-up/down resistor for the given pin.
  --/ However, it is usually more convenient to use bcm2835_gpio_set_pud().
  --/ \param[in] pud The desired Pull-up/down mode. One of BCM2835_GPIO_PUD_* from bcm2835PUDControl
  --/ \sa bcm2835_gpio_set_pud()
   procedure bcm2835_gpio_pud (pud : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:977
   pragma Import (C, bcm2835_gpio_pud, "bcm2835_gpio_pud");

  --/ Clocks the Pull-up/down value set earlier by bcm2835_gpio_pud() into the pin.
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  --/ \param[in] on HIGH to clock the value from bcm2835_gpio_pud() into the pin. 
  --/ LOW to remove the clock. 
  --/ \sa bcm2835_gpio_set_pud()
   procedure bcm2835_gpio_pudclk (pin : stdint_h.uint8_t; on : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:984
   pragma Import (C, bcm2835_gpio_pudclk, "bcm2835_gpio_pudclk");

  --/ Reads and returns the Pad Control for the given GPIO group.
  --/ \param[in] group The GPIO pad group number, one of BCM2835_PAD_GROUP_GPIO_*
  --/ \return Mask of bits from BCM2835_PAD_* from \ref bcm2835PadGroup
   function bcm2835_gpio_pad (group : stdint_h.uint8_t) return stdint_h.uint32_t;  -- /usr/local/include/bcm2835.h:989
   pragma Import (C, bcm2835_gpio_pad, "bcm2835_gpio_pad");

  --/ Sets the Pad Control for the given GPIO group.
  --/ \param[in] group The GPIO pad group number, one of BCM2835_PAD_GROUP_GPIO_*
  --/ \param[in] control Mask of bits from BCM2835_PAD_* from \ref bcm2835PadGroup. Note 
  --/ that it is not necessary to include BCM2835_PAD_PASSWRD in the mask as this
  --/ is automatically included.
   procedure bcm2835_gpio_set_pad (group : stdint_h.uint8_t; control : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:996
   pragma Import (C, bcm2835_gpio_set_pad, "bcm2835_gpio_set_pad");

  --/ Delays for the specified number of milliseconds.
  --/ Uses nanosleep(), and therefore does not use CPU until the time is up.
  --/ However, you are at the mercy of nanosleep(). From the manual for nanosleep():
  --/ If the interval specified in req is not an exact multiple of the granularity  
  --/ underlying  clock  (see  time(7)),  then the interval will be
  --/ rounded up to the next multiple. Furthermore, after the sleep completes, 
  --/ there may still be a delay before the CPU becomes free to once
  --/ again execute the calling thread.
  --/ \param[in] millis Delay in milliseconds
   procedure bcm2835_delay (millis : unsigned);  -- /usr/local/include/bcm2835.h:1007
   pragma Import (C, bcm2835_delay, "bcm2835_delay");

  --/ Delays for the specified number of microseconds.
  --/ Uses a combination of nanosleep() and a busy wait loop on the BCM2835 system timers,
  --/ However, you are at the mercy of nanosleep(). From the manual for nanosleep():
  --/ If the interval specified in req is not an exact multiple of the granularity  
  --/ underlying  clock  (see  time(7)),  then the interval will be
  --/ rounded up to the next multiple. Furthermore, after the sleep completes, 
  --/ there may still be a delay before the CPU becomes free to once
  --/ again execute the calling thread.
  --/ For times less than about 450 microseconds, uses a busy wait on the System Timer.
  --/ It is reported that a delay of 0 microseconds on RaspberryPi will in fact
  --/ result in a delay of about 80 microseconds. Your mileage may vary.
  --/ \param[in] micros Delay in microseconds
   procedure bcm2835_delayMicroseconds (micros : stdint_h.uint64_t);  -- /usr/local/include/bcm2835.h:1021
   pragma Import (C, bcm2835_delayMicroseconds, "bcm2835_delayMicroseconds");

  --/ Sets the output state of the specified pin
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  --/ \param[in] on HIGH sets the output to HIGH and LOW to LOW.
   procedure bcm2835_gpio_write (pin : stdint_h.uint8_t; on : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1026
   pragma Import (C, bcm2835_gpio_write, "bcm2835_gpio_write");

  --/ Sets any of the first 32 GPIO output pins specified in the mask to the state given by on
  --/ \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
  --/ \param[in] on HIGH sets the output to HIGH and LOW to LOW.
   procedure bcm2835_gpio_write_multi (mask : stdint_h.uint32_t; on : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1031
   pragma Import (C, bcm2835_gpio_write_multi, "bcm2835_gpio_write_multi");

  --/ Sets the first 32 GPIO output pins specified in the mask to the value given by value
  --/ \param[in] value values required for each bit masked in by mask, eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
  --/ \param[in] mask Mask of pins to affect. Use eg: (1 << RPI_GPIO_P1_03) | (1 << RPI_GPIO_P1_05)
   procedure bcm2835_gpio_write_mask (value : stdint_h.uint32_t; mask : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:1036
   pragma Import (C, bcm2835_gpio_write_mask, "bcm2835_gpio_write_mask");

  --/ Sets the Pull-up/down mode for the specified pin. This is more convenient than
  --/ clocking the mode in with bcm2835_gpio_pud() and bcm2835_gpio_pudclk().
  --/ \param[in] pin GPIO number, or one of RPI_GPIO_P1_* from \ref RPiGPIOPin.
  --/ \param[in] pud The desired Pull-up/down mode. One of BCM2835_GPIO_PUD_* from bcm2835PUDControl
   procedure bcm2835_gpio_set_pud (pin : stdint_h.uint8_t; pud : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1042
   pragma Import (C, bcm2835_gpio_set_pud, "bcm2835_gpio_set_pud");

  --/ @} 
  --/ \defgroup spi SPI access
  --/ These functions let you use SPI0 (Serial Peripheral Interface) to 
  --/ interface with an external SPI device.
  --/ @{
  --/ Start SPI operations.
  --/ Forces RPi SPI0 pins P1-19 (MOSI), P1-21 (MISO), P1-23 (CLK), P1-24 (CE0) and P1-26 (CE1)
  --/ to alternate function ALT0, which enables those pins for SPI interface.
  --/ You should call bcm2835_spi_end() when all SPI funcitons are complete to return the pins to 
  --/ their default functions
  --/ \sa  bcm2835_spi_end()
   procedure bcm2835_spi_begin;  -- /usr/local/include/bcm2835.h:1057
   pragma Import (C, bcm2835_spi_begin, "bcm2835_spi_begin");

  --/ End SPI operations.
  --/ SPI0 pins P1-19 (MOSI), P1-21 (MISO), P1-23 (CLK), P1-24 (CE0) and P1-26 (CE1)
  --/ are returned to their default INPUT behaviour.
   procedure bcm2835_spi_end;  -- /usr/local/include/bcm2835.h:1062
   pragma Import (C, bcm2835_spi_end, "bcm2835_spi_end");

  --/ Sets the SPI bit order
  --/ NOTE: has no effect. Not supported by SPI0.
  --/ Defaults to 
  --/ \param[in] order The desired bit order, one of BCM2835_SPI_BIT_ORDER_*, 
  --/ see \ref bcm2835SPIBitOrder
   procedure bcm2835_spi_setBitOrder (order : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1069
   pragma Import (C, bcm2835_spi_setBitOrder, "bcm2835_spi_setBitOrder");

  --/ Sets the SPI clock divider and therefore the 
  --/ SPI clock speed. 
  --/ \param[in] divider The desired SPI clock divider, one of BCM2835_SPI_CLOCK_DIVIDER_*, 
  --/ see \ref bcm2835SPIClockDivider
   procedure bcm2835_spi_setClockDivider (divider : stdint_h.uint16_t);  -- /usr/local/include/bcm2835.h:1075
   pragma Import (C, bcm2835_spi_setClockDivider, "bcm2835_spi_setClockDivider");

  --/ Sets the SPI data mode
  --/ Sets the clock polariy and phase
  --/ \param[in] mode The desired data mode, one of BCM2835_SPI_MODE*, 
  --/ see \ref bcm2835SPIMode
   procedure bcm2835_spi_setDataMode (mode : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1081
   pragma Import (C, bcm2835_spi_setDataMode, "bcm2835_spi_setDataMode");

  --/ Sets the chip select pin(s)
  --/ When an bcm2835_spi_transfer() is made, the selected pin(s) will be asserted during the
  --/ transfer.
  --/ \param[in] cs Specifies the CS pins(s) that are used to activate the desired slave. 
  --/   One of BCM2835_SPI_CS*, see \ref bcm2835SPIChipSelect
   procedure bcm2835_spi_chipSelect (cs : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1088
   pragma Import (C, bcm2835_spi_chipSelect, "bcm2835_spi_chipSelect");

  --/ Sets the chip select pin polarity for a given pin
  --/ When an bcm2835_spi_transfer() occurs, the currently selected chip select pin(s) 
  --/ will be asserted to the 
  --/ value given by active. When transfers are not happening, the chip select pin(s) 
  --/ return to the complement (inactive) value.
  --/ \param[in] cs The chip select pin to affect
  --/ \param[in] active Whether the chip select pin is to be active HIGH
   procedure bcm2835_spi_setChipSelectPolarity (cs : stdint_h.uint8_t; active : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1097
   pragma Import (C, bcm2835_spi_setChipSelectPolarity, "bcm2835_spi_setChipSelectPolarity");

  --/ Transfers one byte to and from the currently selected SPI slave.
  --/ Asserts the currently selected CS pins (as previously set by bcm2835_spi_chipSelect) 
  --/ during the transfer.
  --/ Clocks the 8 bit value out on MOSI, and simultaneously clocks in data from MISO. 
  --/ Returns the read data byte from the slave.
  --/ Uses polled transfer as per section 10.6.1 of the BCM 2835 ARM Peripherls manual
  --/ \param[in] value The 8 bit data byte to write to MOSI
  --/ \return The 8 bit byte simultaneously read from  MISO
  --/ \sa bcm2835_spi_transfern()
   function bcm2835_spi_transfer (value : stdint_h.uint8_t) return stdint_h.uint8_t;  -- /usr/local/include/bcm2835.h:1108
   pragma Import (C, bcm2835_spi_transfer, "bcm2835_spi_transfer");

  --/ Transfers any number of bytes to and from the currently selected SPI slave.
  --/ Asserts the currently selected CS pins (as previously set by bcm2835_spi_chipSelect) 
  --/ during the transfer.
  --/ Clocks the len 8 bit bytes out on MOSI, and simultaneously clocks in data from MISO. 
  --/ The data read read from the slave is placed into rbuf. rbuf must be at least len bytes long
  --/ Uses polled transfer as per section 10.6.1 of the BCM 2835 ARM Peripherls manual
  --/ \param[in] tbuf Buffer of bytes to send. 
  --/ \param[out] rbuf Received bytes will by put in this buffer
  --/ \param[in] len Number of bytes in the tbuf buffer, and the number of bytes to send/received
  --/ \sa bcm2835_spi_transfer()
   procedure bcm2835_spi_transfernb
     (tbuf : Interfaces.C.Strings.chars_ptr;
      rbuf : Interfaces.C.Strings.chars_ptr;
      len : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:1120
   pragma Import (C, bcm2835_spi_transfernb, "bcm2835_spi_transfernb");

  --/ Transfers any number of bytes to and from the currently selected SPI slave
  --/ using bcm2835_spi_transfernb.
  --/ The returned data from the slave replaces the transmitted data in the buffer.
  --/ \param[in,out] buf Buffer of bytes to send. Received bytes will replace the contents
  --/ \param[in] len Number of bytes int eh buffer, and the number of bytes to send/received
  --/ \sa bcm2835_spi_transfer()
   procedure bcm2835_spi_transfern (buf : Interfaces.C.Strings.chars_ptr; len : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:1128
   pragma Import (C, bcm2835_spi_transfern, "bcm2835_spi_transfern");

  --/ Transfers any number of bytes to the currently selected SPI slave.
  --/ Asserts the currently selected CS pins (as previously set by bcm2835_spi_chipSelect)
  --/ during the transfer.
  --/ \param[in] buf Buffer of bytes to send.
  --/ \param[in] len Number of bytes in the tbuf buffer, and the number of bytes to send
   procedure bcm2835_spi_writenb (buf : Interfaces.C.Strings.chars_ptr; len : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:1135
   pragma Import (C, bcm2835_spi_writenb, "bcm2835_spi_writenb");

  --/ @}
  --/ \defgroup i2c I2C access
  --/ These functions let you use I2C (The Broadcom Serial Control bus with the Philips
  --/ I2C bus/interface version 2.1 January 2000.) to interface with an external I2C device.
  --/ @{
  --/ Start I2C operations.
  --/ Forces RPi I2C pins P1-03 (SDA) and P1-05 (SCL)
  --/ to alternate function ALT0, which enables those pins for I2C interface.
  --/ You should call bcm2835_i2c_end() when all I2C functions are complete to return the pins to
  --/ their default functions
  --/ \sa  bcm2835_i2c_end()
   procedure bcm2835_i2c_begin;  -- /usr/local/include/bcm2835.h:1150
   pragma Import (C, bcm2835_i2c_begin, "bcm2835_i2c_begin");

  --/ End I2C operations.
  --/ I2C pins P1-03 (SDA) and P1-05 (SCL)
  --/ are returned to their default INPUT behaviour.
   procedure bcm2835_i2c_end;  -- /usr/local/include/bcm2835.h:1155
   pragma Import (C, bcm2835_i2c_end, "bcm2835_i2c_end");

  --/ Sets the I2C slave address.
  --/ \param[in] addr The I2C slave address.
   procedure bcm2835_i2c_setSlaveAddress (addr : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1159
   pragma Import (C, bcm2835_i2c_setSlaveAddress, "bcm2835_i2c_setSlaveAddress");

  --/ Sets the I2C clock divider and therefore the I2C clock speed.
  --/ \param[in] divider The desired I2C clock divider, one of BCM2835_I2C_CLOCK_DIVIDER_*,
  --/ see \ref bcm2835I2CClockDivider
   procedure bcm2835_i2c_setClockDivider (divider : stdint_h.uint16_t);  -- /usr/local/include/bcm2835.h:1164
   pragma Import (C, bcm2835_i2c_setClockDivider, "bcm2835_i2c_setClockDivider");

  --/ Sets the I2C clock divider by converting the baudrate parameter to
  --/ the equivalent I2C clock divider. ( see \sa bcm2835_i2c_setClockDivider)
  --/ For the I2C standard 100khz you would set baudrate to 100000
  --/ The use of baudrate corresponds to its use in the I2C kernel device
  --/ driver. (Of course, bcm2835 has nothing to do with the kernel driver)
   procedure bcm2835_i2c_set_baudrate (baudrate : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:1171
   pragma Import (C, bcm2835_i2c_set_baudrate, "bcm2835_i2c_set_baudrate");

  --/ Transfers any number of bytes to the currently selected I2C slave.
  --/ (as previously set by \sa bcm2835_i2c_setSlaveAddress)
  --/ \param[in] buf Buffer of bytes to send.
  --/ \param[in] len Number of bytes in the buf buffer, and the number of bytes to send.
  --/ \return reason see \ref bcm2835I2CReasonCodes
   function bcm2835_i2c_write (buf : Interfaces.C.Strings.chars_ptr; len : stdint_h.uint32_t) return stdint_h.uint8_t;  -- /usr/local/include/bcm2835.h:1178
   pragma Import (C, bcm2835_i2c_write, "bcm2835_i2c_write");

  --/ Transfers any number of bytes from the currently selected I2C slave.
  --/ (as previously set by \sa bcm2835_i2c_setSlaveAddress)
  --/ \param[in] buf Buffer of bytes to receive.
  --/ \param[in] len Number of bytes in the buf buffer, and the number of bytes to received.
  --/ \return reason see \ref bcm2835I2CReasonCodes
   function bcm2835_i2c_read (buf : Interfaces.C.Strings.chars_ptr; len : stdint_h.uint32_t) return stdint_h.uint8_t;  -- /usr/local/include/bcm2835.h:1185
   pragma Import (C, bcm2835_i2c_read, "bcm2835_i2c_read");

  --/ Allows reading from I2C slaves that require a repeated start (without any prior stop)
  --/ to read after the required slave register has been set. For example, the popular
  --/ MPL3115A2 pressure and temperature sensor. Note that your device must support or
  --/ require this mode. If your device does not require this mode then the standard
  --/ combined:
  --/   \sa bcm2835_i2c_write
  --/   \sa bcm2835_i2c_read
  --/ are a better choice.
  --/ Will read from the slave previously set by \sa bcm2835_i2c_setSlaveAddress
  --/ \param[in] regaddr Buffer containing the slave register you wish to read from.
  --/ \param[in] buf Buffer of bytes to receive.
  --/ \param[in] len Number of bytes in the buf buffer, and the number of bytes to received.
  --/ \return reason see \ref bcm2835I2CReasonCodes
   function bcm2835_i2c_read_register_rs
     (regaddr : Interfaces.C.Strings.chars_ptr;
      buf : Interfaces.C.Strings.chars_ptr;
      len : stdint_h.uint32_t) return stdint_h.uint8_t;  -- /usr/local/include/bcm2835.h:1200
   pragma Import (C, bcm2835_i2c_read_register_rs, "bcm2835_i2c_read_register_rs");

  --/ Allows sending an arbitrary number of bytes to I2C slaves before issuing a repeated
  --/ start (with no prior stop) and reading a response.
  --/ Necessary for devices that require such behavior, such as the MLX90620.
  --/ Will write to and read from the slave previously set by \sa bcm2835_i2c_setSlaveAddress
  --/ \param[in] cmds Buffer containing the bytes to send before the repeated start condition.
  --/ \param[in] cmds_len Number of bytes to send from cmds buffer
  --/ \param[in] buf Buffer of bytes to receive.
  --/ \param[in] buf_len Number of bytes to receive in the buf buffer.
  --/ \return reason see \ref bcm2835I2CReasonCodes
   function bcm2835_i2c_write_read_rs
     (cmds : Interfaces.C.Strings.chars_ptr;
      cmds_len : stdint_h.uint32_t;
      buf : Interfaces.C.Strings.chars_ptr;
      buf_len : stdint_h.uint32_t) return stdint_h.uint8_t;  -- /usr/local/include/bcm2835.h:1211
   pragma Import (C, bcm2835_i2c_write_read_rs, "bcm2835_i2c_write_read_rs");

  --/ @}
  --/ \defgroup st System Timer access
  --/ Allows access to and delays using the System Timer Counter.
  --/ @{
  --/ Read the System Timer Counter register.
  --/ \return the value read from the System Timer Counter Lower 32 bits register
   function bcm2835_st_read return stdint_h.uint64_t;  -- /usr/local/include/bcm2835.h:1221
   pragma Import (C, bcm2835_st_read, "bcm2835_st_read");

  --/ Delays for the specified number of microseconds with offset.
  --/ \param[in] offset_micros Offset in microseconds
  --/ \param[in] micros Delay in microseconds
   procedure bcm2835_st_delay (offset_micros : stdint_h.uint64_t; micros : stdint_h.uint64_t);  -- /usr/local/include/bcm2835.h:1226
   pragma Import (C, bcm2835_st_delay, "bcm2835_st_delay");

  --/ @} 
  --/ \defgroup pwm Pulse Width Modulation
  --/ Allows control of 2 independent PWM channels. A limited subset of GPIO pins
  --/ can be connected to one of these 2 channels, allowing PWM control of GPIO pins.
  --/ You have to set the desired pin into a particular Alt Fun to PWM output. See the PWM
  --/ documentation on the Main Page.
  --/ @{
  --/ Sets the PWM clock divisor, 
  --/ to control the basic PWM pulse widths.
  --/ \param[in] divisor Divides the basic 19.2MHz PWM clock. You can use one of the common
  --/ values BCM2835_PWM_CLOCK_DIVIDER_* in \ref bcm2835PWMClockDivider
   procedure bcm2835_pwm_set_clock (divisor : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:1241
   pragma Import (C, bcm2835_pwm_set_clock, "bcm2835_pwm_set_clock");

  --/ Sets the mode of the given PWM channel,
  --/ allowing you to control the PWM mode and enable/disable that channel
  --/ \param[in] channel The PWM channel. 0 or 1.
  --/ \param[in] markspace Set true if you want Mark-Space mode. 0 for Balanced mode.
  --/ \param[in] enabled Set true to enable this channel and produce PWM pulses.
   procedure bcm2835_pwm_set_mode
     (channel : stdint_h.uint8_t;
      markspace : stdint_h.uint8_t;
      enabled : stdint_h.uint8_t);  -- /usr/local/include/bcm2835.h:1248
   pragma Import (C, bcm2835_pwm_set_mode, "bcm2835_pwm_set_mode");

  --/ Sets the maximum range of the PWM output.
  --/ The data value can vary between 0 and this range to control PWM output
  --/ \param[in] channel The PWM channel. 0 or 1.
  --/ \param[in] range The maximum value permitted for DATA.
   procedure bcm2835_pwm_set_range (channel : stdint_h.uint8_t; c_range : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:1254
   pragma Import (C, bcm2835_pwm_set_range, "bcm2835_pwm_set_range");

  --/ Sets the PWM pulse ratio to emit to DATA/RANGE, 
  --/ where RANGE is set by bcm2835_pwm_set_range().
  --/ \param[in] channel The PWM channel. 0 or 1.
  --/ \param[in] data Controls the PWM output ratio as a fraction of the range. 
  --/  Can vary from 0 to RANGE.
   procedure bcm2835_pwm_set_data (channel : stdint_h.uint8_t; data : stdint_h.uint32_t);  -- /usr/local/include/bcm2835.h:1261
   pragma Import (C, bcm2835_pwm_set_data, "bcm2835_pwm_set_data");

  --/ @} 
  --/ @example blink.c
  --/ Blinks RPi GPIO pin 11 on and off
  --/ @example input.c
  --/ Reads the state of an RPi input pin
  --/ @example event.c
  --/ Shows how to use event detection on an input pin
  --/ @example spi.c
  --/ Shows how to use SPI interface to transfer a byte to and from an SPI device
  --/ @example spin.c
  --/ Shows how to use SPI interface to transfer a number of bytes to and from an SPI device
  --/ @example pwm.c
  --/ Shows how to use PWM to control GPIO pins
  --/ @example i2c.c
  --/ Command line utility for executing i2c commands with the 
  --/ Broadcom bcm2835. Contributed by Shahrooz Shahparnia.
  --/ example gpio.c
  --/ Command line utility for executing gpio commands with the 
  --/   Broadcom bcm2835. Contributed by Shahrooz Shahparnia.
end bcm2835_h;
