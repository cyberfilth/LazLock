==SplashAbout Properties Tester==

Once splashabout has been installed on your component palette, then project1 should compile in windows and linux.

Note: the compiler messages 'Warning: Symbol x is not portable' are not errors.
It is because the shaped 'splash' screen features are unavailable outside the 
Windows environment, and the corresponding properties are tagged as such.

I have included some sample graphic files to demonstrate the external file options
cockroach.bmp/jpg, golfballs.bmp/jpg and sunflower.bmp/jpg are 
MaskColorImage and MaskMonoImage pairs
IMPORTANT: if you choose to specify external files, then you 
MUST deploy them with your executable in the same folder

If you choose Graphics=saResources (default) then no external files are required.

Open license.txt to see how to write a user-defined license.
Try changing the TitleStyle, Graphics and MaskType properties to experiment

==If you are reading this via the [Help] button in the SplashAbout properties tester Application==

The SplashAbout tester displays all the current properties for the loaded SplashAbout1 component.

All changes you make will only last whilst the app is open.
If you want the app to load with your own property values, then open 
 up the mainform (Unit1) of the app and click the SplashAbout component
 in order to make permanent changes via the Object Inspector.

The purpose of the 'SplashAbout properties tester' is so that you can 
experiment with the many properties of SplashAbout, testing 
 (via the [Test Splash] and [Test About] buttons) until you are
 familiar with configuring the component.

Change one property at a time, than click the [Test Splash] and [Test About]
buttons to see the effect of your changes.  Remember to test the [Credits]
and [License] buttons on the About dialog as well.

You can also use it as a prototyper for your own 
application's Splash and About screens.

If you choose to edit 'license.txt' and clear it's contents before saving it,
then the original 'license.txt' will be backed up to 'license.txt.bak'
Thus, you can restore the original example if you wish at a later date.

To make a MaskMonoImage
======================
In an image editor (I used Paint Shop Pro) reduce the palette to 2 colours and save as a BMP file (Not RLEncoded)
Black=transparent to the desktop
White=transparent to the splash window background (can be a solid colour or a BackgroundImage or a MaskPImage)
Note: If a MaskColorImage is specified, it hides the BackGroundImage in the splash screen (but not the About dialog)
and also supresses the text display on the splash screen (see below)

To make a MaskMonoImage and MaskColorImage pair
==========================================
For the most dramatic effect, find an strong image which fits fully within a square window 
without touching the edges (compare images 'cockroach' and 'golfballs' with 'sunflower')
Using a paint program isolate the image on a white background and save it as a jpg
Using a paint program, make the image pure black, reduce the colour pallete to 2 and save it as a BMP
Set the properties:
MaskType=saUserImage
Graphics=saResources
BitmapMaskColor=your jpg file
BitmapMaskMono=your bmp file


If all this looks complicated - just experiment with the properties and images provided!
