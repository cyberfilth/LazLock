                __             __            _     
               / /  __ _ ____ / /  ___   ___| | __
              / /  / _` |_  // /  / _ \ / __| |/ /
             / /__| (_| |/ // /__| (_) | (__|   < 
             \____/\__,_/___\____/\___/ \___|_|\_\
                                     
01001100 01100001 01111010 01001100 01101111 01100011 01101011



Introduction
------------
LazLock is a lightweight, portable password manager that runs on both Windows and GNU/Linux.
It doesn't need to be installed on your computer, so can be run from a USB stick. Carry both the Windows and the Linux versions with you and you'll be able to access your passwords from any PC.
It also has an option to create strong, random passwords for all of the websites that you visit, you only need to remember one password to unlock them all. 
Your data is strongly protected with 128 bit AES encryption.


Features
--------
+ 128 bit AES encryption
+ Portable application with no need to install
+ Simple interface
+ Fast decryption on the fly
+ Cross platform
+ A plain text version of your data is never written to disk as all decryption is done in memory.


Usage
-----
When you run the program for the first time, you'll be prompted for a password. This password will be used to protect all of your login details so it should be a strong password but also easy to remember (it will also be the ONLY password that you'll have to remember from now on).
LazLock will then create a 'vault' file to securely store your data.

You can now start entering your login details for the various websites that you visit.
Each entry is sorted by category (Email, Work, Social Media etc.) to make it easier to find as your list of passwords grows.

Create more secure passwords by clicking on the 'Password Generator' icon and randomly creating a longer, harder to crack password.
Remember that you only need to remember one password, the one used to unlock LazLock, so make your other logins as secure as possible.

There is an option to create an unencrypted backup via 'Export as plain text' under the File menu.
I believe that this program is secure enough to use daily (and I do) but if you ever forget the password to unlock your vault then your files will stay locked and encrypted.
Safe, but encrypted.
There are no backdoors to LazLock and no way to access your data without the correct password.

Linux specific usage
--------------------
Linux will not allow you to run a binary from a USB stick that is formatted as FAT32.
If LazLock will not run on your computer, first make sure that it is executable. Open a terminal and navigate to the directory where LazLock is saved.
Enter chmod +x lazlock
Then try running the program by entering ./lazlock


Help
----
Any bugs can be reported on the sourceforge page at https://sourceforge.net/p/lazlock/tickets/

The latest version of LazLock can be downloaded from https://sourceforge.net/projects/lazlock/


Contact
-------
You can contact the author by email at cyberfilth@protonmail.com
or follow him on Twitter at @CyberFilth


Translations
------------
Portugese & Brazilian translations - Gustavo Carreno
Polish Translation - Aleksandra Hawkins

If you'd like to help translating LazLock into other languages, please contact Gustavo Carreno at https://github.com/gcarreno/lazlock-l10n




Copyright (C) 2016-2019 Chris Hawkins, LazLock is licensed under the MIT license.

Readme.txt last updated 21 November 2017
