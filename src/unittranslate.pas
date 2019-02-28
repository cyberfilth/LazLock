unit unittranslate;

{$mode objfpc}{$H+}

interface


resourcestring

  CategorySelect = 'Select';
  MSGareYouSure = 'Are you sure you want to delete ';
  MSGsaveChanges = 'Save changes?';
  MSGfileHasBeenChanged = 'Your LazLock file has been changed.' +
    #13#10 + 'Do you want to save before you exit?';
  NoPasswordExit = 'No password entered, LazLock will exit';
  CreateCaption = 'Create a new file';
  NewVaultMessage = 'A new LazLock vault will be created.' + #13#10 +
    'Please enter a new password that will be used to protect your data.' +
    #13#10 + #13#10 + 'IT IS IMPORTANT THAT YOU NEVER FORGET THIS PASSWORD.' +
    #13#10 + 'To protect your privacy, LazLock will not create a backup of your password.';
  ConfirmCaption = 'Confirm password';
  ReenterPW = 'Please re-enter the password to confirm.';
  DontMatch = 'Passwords do not match' + #13#10 + 'LazLock will now exit';
  EnterPWCaption = 'Enter password';
  EnterPW = 'Please enter your password.';
  ExitDecryptMessage = 'Incorrect password entered.' + #13#10 + 'LazLock will now quit.';
  ChangeCaption = 'Change password';
  NewPW1Message = 'Enter a new password to encrypt this file.';
  NoChangeMessage = 'Passwords do not match.' + #13#10 +
    'Current password has not been changed.';
  OpenLinkDefaultBrowser = 'Open link in default web browser';
  CopyToCBoard = 'Copy to the clipboard';
  AddEntryPWGeneratorCaption = 'Add Entry / Password Generator';
  EditEntryPWGeneratorCaption = 'Edit Entry / Password Generator';
  PWgeneratorCaption = 'Password Generator';
  NoEntryMessage = 'No entry selected';
  LetsBegin = 'Let''s begin';
  WelcomeMessageDialog = 'Welcome to LazLock Password Manager' +
    #13#10 + 'A new vault has been created for you to begin storing your passwords.' +
    #13#10 + 'Do you wish to view the online help?';
  OpenPWgeneratorCaption = 'Open Password Generator';
  STATUStotalRecords = ' records in LazLock';
  STATUSsaveEncrypt = 'Save and encrypt database';
  STATUSremoveEntry = 'Remove an entry';
  STATUSexitLazLock = 'Exit LazLock';
  STATUSeditEntry = 'Edit an entry';
  STATUSaddEntry = 'Add a new entry';
  STATUSenableSpaces =
    'Enable spaces   *note: Not all websites allow spaces in passwords';
  STATUSenableNumbers = 'Enable numbers in password';
  STATUSenableSymbols = 'Enable symbols / special characters in password';
  STATUSenableUpperCase = 'Enable uppercase letters in password';
  STATUSenableLowerCase = 'Enable lowercase letters in password';
  STATUSenableBrackets = 'Enable brackets in password';
  STATUSselectPWlength = 'Select number of characters in password';
  CompleteAllFields = 'Please complete all fields to add a new entry';
  ClosePWgenerator = 'Close password generator';
  CopyPWtoClipboard = 'Copy password to clipboard';
  SelectPWcharacters = 'Please select which characters to include in your password.';
  VeryWeakPW = 'Very weak password';
  WeakPW = 'Weak password';
  AveragePW = 'Average strength password';
  StrongPW = 'Strong password';
  VeryStrongPW = 'Very strong password';
  GenerateRandomPW = 'Generate a random password';
  lblAll = 'All';
  lblBanking = 'Banking';
  lblEducation = 'Education';
  lblEmail = 'Email';
  lblMedia = 'Media';
  lblOther = 'Other';
  lblShopping = 'Shopping';
  lblSocialMedia = 'Social Media';
  lblSoftware = 'Software';
  lblWeb = 'Web';
  lblWork = 'Work';

implementation

end.
