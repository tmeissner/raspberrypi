# raspilcd

## dependencies

* BCM2835 library: [(http://www.airspayce.com/mikem/bcm2835/](http://www.airspayce.com/mikem/bcm2835/index.html)
* GNAT compiler: `sudo apt-get install gnat`

## building

`gnat gnatmake raspilcd.adb -largs /usr/local/lib/libbcm2835.a`

## using

`sudo ./raspilcd test.bmp`