# raspilcd

## dependencies

* BCM2835 library: [http://www.airspayce.com/mikem/bcm2835/](http://www.airspayce.com/mikem/bcm2835/index.html)
* GNAT compiler: `sudo apt-get install gnat`

## building

`gnat gnatmake raspilcd.adb -largs /usr/local/lib/libbcm2835.a`

## using

`sudo ./raspilcd test.bmp`

![image](https://lh6.googleusercontent.com/-P-_f2-KJT7M/UtccE_iookI/AAAAAAAACss/cofis5sWc54/w1158-h869-no/lcd_grafik.jpg)