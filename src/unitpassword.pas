(* LazLock Password manager

  Copyright (c) 2016 - 2017 Chris Hawkins

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
*)

unit unitpassword;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, Spin, Math, strutils, LCLIntf
  {$IFDEF Windows}
  , Windows
  {$ENDIF};

type
  CardinalArray = array of cardinal;

  { TPasswordGenerator }

  TPasswordGenerator = class(TForm)
    btnSaveChanges: TButton;
    btnCancel: TButton;
    btnAdd: TButton;
    btnCancel2: TButton;
    Cbox: TCheckBox;
    Dbox: TCheckBox;
    Bbox: TCheckBox;
    Abox: TCheckBox;
    Ebox: TCheckBox;
    edCategory2: TComboBox;
    edImage2: TComboBox;
    edPasswordA: TEdit;
    edUserA: TEdit;
    edURLA: TEdit;
    edNameA: TEdit;
    edPasswordB: TEdit;
    edUserB: TEdit;
    edURLB: TEdit;
    edNameB: TEdit;
    gradientImage2: TImage;
    gradientImage1: TImage;
    edName2: TLabel;
    edURL2: TLabel;
    edUser2: TLabel;
    edPassword2: TLabel;
    edName1: TLabel;
    edURL1: TLabel;
    edUser1: TLabel;
    edPassword1: TLabel;
    gradientImage3: TImage;
    ColourPanel: TPanel;
    solidColour3: TImage;
    StrengthIndicator: TShape;
    solidColour1: TImage;
    solidColour2: TImage;
    Image3: TImage;
    lblCategory2: TLabel;
    lblIcon2: TLabel;
    lblPWStrength: TLabel;
    lblTitle1: TLabel;
    linkPWGenerator2: TLabel;
    Fbox: TCheckBox;
    edCategory: TComboBox;
    edImage: TComboBox;
    PasswordNew: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OptionsGroup: TGroupBox;
    Shield1: TImage;
    linkPWGenerator: TLabel;
    lblCategory: TLabel;
    lblTitle: TLabel;
    lblIcon: TLabel;
    PageControl1: TPageControl;
    btnGenPW: TSpeedButton;
    btnCopy: TSpeedButton;
    btnClose: TSpeedButton;
    SpinEdit1: TSpinEdit;
    StatusBar: TStatusBar;
    tabEnterNew: TTabSheet;
    tabPWGenerator: TTabSheet;
    tabEditEntry: TTabSheet;
    TrayIcon1: TTrayIcon;
    procedure AboxMouseEnter(Sender: TObject);
    procedure BboxMouseEnter(Sender: TObject);
    procedure btnSaveChangesClick(Sender: TObject);
    procedure btnAddClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnCloseMouseEnter(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure btnCopyMouseEnter(Sender: TObject);
    procedure btnGenPWClick(Sender: TObject);
    procedure btnGenPWMouseEnter(Sender: TObject);
    procedure CboxMouseEnter(Sender: TObject);
    procedure DboxMouseEnter(Sender: TObject);
    procedure EboxMouseEnter(Sender: TObject);
    procedure edCategoryChange(Sender: TObject);
    procedure edImage2Change(Sender: TObject);
    procedure edImage2DrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure edImageChange(Sender: TObject);
    procedure edImageDrawItem(Control: TWinControl; Index: integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FboxMouseEnter(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure linkPWGeneratorClick(Sender: TObject);
    procedure ClearAndClose;
    procedure ClearStatusBar(Sender: TObject);
    procedure SpinEdit1MouseEnter(Sender: TObject);
    procedure PasswordStrengthMeter;
    procedure TrayIcon1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

const
  // Strings used to generate password
  A: string = '!#%+:;"=_-?@^*&$';
  B: string = '1234567890';
  C: string = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  D: string = 'abcdefghijklmnopqrstuvwxyz';
  E: string = '()[]{}<>';
  F: string = ' ';

var
  PasswordGenerator: TPasswordGenerator;
  enteredCategory, enteredName, enteredURL, enteredUser, enteredPassword, image: string;
  Chars: integer;
  str: UTF8string;
  EntropyResult: double;

implementation

uses
  unitmain, unittranslate;

{$R *.lfm}

{ TPasswordGenerator }

procedure TPasswordGenerator.FormCreate(Sender: TObject);
begin
  //Clear records
  enteredName := '';
  enteredURL := '';
  enteredUser := '';
  enteredPassword := '';
  // Fill dropdown box with icons
  edImage.Items.Clear;
  edImage.Items.Add('  Website');
  edImage.Items.Add('  Email');
  edImage.Items.Add('  Linux');
  edImage.Items.Add('  BSD');
  edImage.Items.Add('  Amazon');
  edImage.Items.Add('  Blogger');
  edImage.Items.Add('  Delicious');
  edImage.Items.Add('  DeviantArt');
  edImage.Items.Add('  Digg');
  edImage.Items.Add('  Drupal');
  edImage.Items.Add('  DuckDuckGo');
  edImage.Items.Add('  eBay');
  edImage.Items.Add('  Facebook');
  edImage.Items.Add('  Games');
  edImage.Items.Add('  Gmail');
  edImage.Items.Add('  Google');
  edImage.Items.Add('  Google+');
  edImage.Items.Add('  Last.fm');
  edImage.Items.Add('  Linkedin');
  edImage.Items.Add('  Lock');
  edImage.Items.Add('  Money_Banknote');
  edImage.Items.Add('  Money_Bitcoin');
  edImage.Items.Add('  Money_Dollar');
  edImage.Items.Add('  Money_Pound');
  edImage.Items.Add('  PayPal');
  edImage.Items.Add('  Picasa');
  edImage.Items.Add('  Pinterest');
  edImage.Items.Add('  Purple Heart');
  edImage.Items.Add('  Reddit');
  edImage.Items.Add('  RSS');
  edImage.Items.Add('  Skype');
  edImage.Items.Add('  Slashdot');
  edImage.Items.Add('  StumbleUpon');
  edImage.Items.Add('  Technorati');
  edImage.Items.Add('  Tumblr');
  edImage.Items.Add('  Twitter');
  edImage.Items.Add('  User');
  edImage.Items.Add('  Vimeo');
  edImage.Items.Add('  WordPress');
  edImage.Items.Add('  YouTube');
  edImage.ItemIndex := -1;
  // Fill 2nd dropdown box with categories
  edCategory.Items.Clear;
  edCategory.Items.Add(lblBanking);
  edCategory.Items.Add(lblEducation);
  edCategory.Items.Add(lblEmail);
  edCategory.Items.Add(lblMedia);
  edCategory.Items.Add(lblOther);
  edCategory.Items.Add(lblShopping);
  edCategory.Items.Add(lblSocialMedia);
  edCategory.Items.Add(lblSoftware);
  edCategory.Items.Add(lblWeb);
  edCategory.Items.Add(lblWork);
  //Edit Entry tab
  edCategory2.Items.Clear;
  edCategory2.Items.Add(lblBanking);
  edCategory2.Items.Add(lblEducation);
  edCategory2.Items.Add(lblEmail);
  edCategory2.Items.Add(lblMedia);
  edCategory2.Items.Add(lblOther);
  edCategory2.Items.Add(lblShopping);
  edCategory2.Items.Add(lblSocialMedia);
  edCategory2.Items.Add(lblSoftware);
  edCategory2.Items.Add(lblWeb);
  edCategory2.Items.Add(lblWork);
  // Site icons
  edImage2.Items.Clear;
  edImage2.Items.Add('  Website');
  edImage2.Items.Add('  Email');
  edImage2.Items.Add('  Linux');
  edImage2.Items.Add('  BSD');
  edImage2.Items.Add('  Amazon');
  edImage2.Items.Add('  Blogger');
  edImage2.Items.Add('  Delicious');
  edImage2.Items.Add('  DeviantArt');
  edImage2.Items.Add('  Digg');
  edImage2.Items.Add('  Drupal');
  edImage2.Items.Add('  DuckDuckGo');
  edImage2.Items.Add('  eBay');
  edImage2.Items.Add('  Facebook');
  edImage2.Items.Add('  Games');
  edImage2.Items.Add('  Gmail');
  edImage2.Items.Add('  Google');
  edImage2.Items.Add('  Google+');
  edImage2.Items.Add('  Last.fm');
  edImage2.Items.Add('  Linkedin');
  edImage2.Items.Add('  Lock');
  edImage2.Items.Add('  Money_Banknote');
  edImage2.Items.Add('  Money_Bitcoin');
  edImage2.Items.Add('  Money_Dollar');
  edImage2.Items.Add('  Money_Pound');
  edImage2.Items.Add('  PayPal');
  edImage2.Items.Add('  Picasa');
  edImage2.Items.Add('  Pinterest');
  edImage2.Items.Add('  Purple Heart');
  edImage2.Items.Add('  Reddit');
  edImage2.Items.Add('  RSS');
  edImage2.Items.Add('  Skype');
  edImage2.Items.Add('  Slashdot');
  edImage2.Items.Add('  StumbleUpon');
  edImage2.Items.Add('  Technorati');
  edImage2.Items.Add('  Tumblr');
  edImage2.Items.Add('  Twitter');
  edImage2.Items.Add('  User');
  edImage2.Items.Add('  Vimeo');
  edImage2.Items.Add('  WordPress');
  edImage2.Items.Add('  YouTube');

  // Start Random seed
  Randomize;
  {$IFDEF Windows}
  RandSeed := ((RandSeed shl 8) or GetCurrentProcessID) xor GetTickCount64;
  {$ENDIF}
  btnGenPW.Click;
end;

procedure TPasswordGenerator.edImageChange(Sender: TObject);
begin
  case edImage.ItemIndex of
    0: image := '0';
    1: image := '1';
    2: image := '2';
    3: image := '3';
    4: image := '4';
    5: image := '5';
    6: image := '6';
    7: image := '7';
    8: image := '8';
    9: image := '9';
    10: image := '10';
    11: image := '11';
    12: image := '12';
    13: image := '13';
    14: image := '14';
    15: image := '15';
    16: image := '16';
    17: image := '17';
    18: image := '18';
    19: image := '19';
    20: image := '20';
    21: image := '21';
    22: image := '22';
    23: image := '23';
    24: image := '24';
    25: image := '25';
    26: image := '26';
    27: image := '27';
    28: image := '28';
    29: image := '29';
    30: image := '30';
    31: image := '31';
    32: image := '32';
    33: image := '33';
    34: image := '34';
    35: image := '35';
    36: image := '36';
    37: image := '37';
    38: image := '38';
    39: image := '39';
  end;
end;


(*  Password Generator tab  *)

procedure TPasswordGenerator.linkPWGeneratorClick(Sender: TObject);
begin
  PageControl1.ActivePage := tabPWGenerator;
end;

procedure TPasswordGenerator.edImageDrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
begin
  edImage.Canvas.FillRect(ARect);
  edImage.Canvas.TextRect(ARect, 25, ARect.Top, edImage.Items[Index]);
  mainLazLock.sitesImageList.Draw(edImage.Canvas, ARect.Left + 1,
    ARect.Top + 1, Index);
end;

procedure TPasswordGenerator.FboxMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := STATUSenableSpaces;
end;

procedure TPasswordGenerator.btnAddClick(Sender: TObject);
begin     // Check that all fields are completed
  if (edCategory.ItemIndex <> -1) and (edNameA.Text <> '') and
    (edURLA.Text <> '') and (edUserA.Text <> '') and (edPasswordA.Text <> '') and
    (edImage.ItemIndex <> -1) then
  begin
    enteredCategory := edCategory.Text;
    enteredName := edNameA.Text;
    enteredURL := edURLA.Text;
    enteredUser := edUserA.Text;
    enteredPassword := edPasswordA.Text;
    with mainLazLock.ListView1.Items.Add do
    begin
      Caption := enteredCategory;
      SubItems.Add(enteredName);
      SubItems.Add(enteredURL);
      SubItems.Add(enteredUser);
      SubItems.Add(enteredPassword);
      SubItems.Add(image);
      unitmain.IsSaved := False;
      mainLazLock.Caption := 'LazLock *';
    end;
    ClearAndClose;
  end
  else
    ShowMessage(CompleteAllFields);
end;

procedure TPasswordGenerator.BboxMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := STATUSenableNumbers;
end;

procedure TPasswordGenerator.AboxMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := STATUSenableSymbols;
end;

procedure TPasswordGenerator.btnCancelClick(Sender: TObject);
begin
  ClearAndClose;
end;

procedure TPasswordGenerator.btnCloseMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := ClosePWgenerator;
end;

procedure TPasswordGenerator.btnCopyClick(Sender: TObject);
begin
  PasswordNew.SelectAll;
  PasswordNew.CopyToClipboard;
  TrayIcon1.ShowBalloonHint; // Provides visual feedback
end;

procedure TPasswordGenerator.btnCopyMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := CopyPWtoClipboard;
end;

function RandomPassword(PLen: integer): string;  // Fill password string
begin
  Result := '';
  repeat
    Result := Result + str[Random(Length(str)) + 1];
  until (Length(Result) = PLen);
  str := '';
end;

function CountNumberOfUniqueCharacters(PWstr: string): string;
var
  Cnumber: cardinal;
begin
  CountNumberOfUniqueCharacters := '';
  for Cnumber := 1 to length(PWstr) do
    if (PosEx(PWstr[Cnumber], PWstr, Cnumber) > 0) and
      (PosEx(PWstr[Cnumber], CountNumberOfUniqueCharacters, 1) = 0) then
      CountNumberOfUniqueCharacters += PWstr[Cnumber];
end;

procedure TPasswordGenerator.btnGenPWClick(Sender: TObject);
// Generate password criteria based on user selection
begin
  try
    Chars := SpinEdit1.Value; // Set number of characters in password
  except // No action
  end;
  str := '';
  // Select password options
  (*
A = Symbols, B = Numbers, C = Uppercase, D = Lowercase, E = Brackets, F = Space

Password options 'power set'

{A}, {B}, {A, B}, {C}, {A, C}, {B, C}, {A, B, C}, {D}, {A, D}, {B, D}, {A, B, D}, {C, D}, {A, C, D}, {B, C, D}, {A, B, C, D},
{E}, {A, E}, {B, E}, {A, B, E}, {C, E}, {A, C, E}, {B, C, E}, {A, B, C, E}, {D, E}, {A, D, E}, {B, D, E}, {A, B, D, E},
{C, D, E}, {A, C, D, E}, {B, C, D, E}, {A, B, C, D, E}, {F}, {A, F}, {B, F}, {A, B, F}, {C, F}, {A, C, F}, {B, C, F},
{A, B, C, F}, {D, F}, {A, D, F}, {B, D, F}, {A, B, D, F}, {C, D, F}, {A, C, D, F}, {B, C, D, F}, {A, B, C, D, F}, {E, F},
{A, E, F}, {B, E, F}, {A, B, E, F}, {C, E, F}, {A, C, E, F}, {B, C, E, F}, {A, B, C, E, F}, {D, E, F}, {A, D, E, F},
{B, D, E, F}, {A, B, D, E, F}, {C, D, E, F}, {A, C, D, E, F}, {B, C, D, E, F}, {A, B, C, D, E, F}
*)

  if Abox.Checked and not (Bbox.Checked) and not (Cbox.Checked) and
    not (Dbox.Checked) and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := A;
  end
  else if not (Abox.Checked) and Bbox.Checked and not (Cbox.Checked) and
    not (Dbox.Checked) and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := B;
  end
  else if Abox.Checked and Bbox.Checked and not (Cbox.Checked) and
    not (Dbox.Checked) and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := A + B;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and Cbox.Checked and
    not (Dbox.Checked) and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := C;
  end
  else if Abox.Checked and not (Bbox.Checked) and Cbox.Checked and
    not (Dbox.Checked) and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := A + C;
  end
  else if not (Abox.Checked) and Bbox.Checked and Cbox.Checked and
    not (Dbox.Checked) and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := B + C;
  end
  else if Abox.Checked and Bbox.Checked and Cbox.Checked and not
    (Dbox.Checked) and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := A + B + C;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and not (Cbox.Checked) and
    Dbox.Checked and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := D;
  end
  else if Abox.Checked and not (Bbox.Checked) and not (Cbox.Checked) and
    Dbox.Checked and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := A + D;
  end
  else if not (Abox.Checked) and Bbox.Checked and not (Cbox.Checked) and
    Dbox.Checked and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := B + D;
  end
  else if Abox.Checked and Bbox.Checked and not (Cbox.Checked) and
    Dbox.Checked and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := A + B + D;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and Cbox.Checked and
    Dbox.Checked and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := C + D;
  end
  else if Abox.Checked and not (Bbox.Checked) and Cbox.Checked and
    Dbox.Checked and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := A + C + D;
  end
  else if not (Abox.Checked) and Bbox.Checked and Cbox.Checked and
    Dbox.Checked and not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := B + C + D;
  end
  else if Abox.Checked and Bbox.Checked and Cbox.Checked and Dbox.Checked and
    not (Ebox.Checked) and not (Fbox.Checked) then
  begin
    str := A + B + C + D;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and not (Cbox.Checked) and
    not (Dbox.Checked) and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := E;
  end
  else if Abox.Checked and not (Bbox.Checked) and not (Cbox.Checked) and
    not (Dbox.Checked) and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := A + E;
  end
  else if not (Abox.Checked) and Bbox.Checked and not (Cbox.Checked) and
    not (Dbox.Checked) and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := B + E;
  end
  else if Abox.Checked and Bbox.Checked and not (Cbox.Checked) and
    not (Dbox.Checked) and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := A + B + E;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and Cbox.Checked and
    not (Dbox.Checked) and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := C + E;
  end
  else if Abox.Checked and not (Bbox.Checked) and Cbox.Checked and
    not (Dbox.Checked) and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := A + C + E;
  end
  else if not (Abox.Checked) and Bbox.Checked and Cbox.Checked and
    not (Dbox.Checked) and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := B + C + E;
  end
  else if Abox.Checked and Bbox.Checked and Cbox.Checked and not
    (Dbox.Checked) and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := A + B + C + E;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and not (Cbox.Checked) and
    Dbox.Checked and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := D + E;
  end
  else if Abox.Checked and not (Bbox.Checked) and not (Cbox.Checked) and
    Dbox.Checked and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := A + D + E;
  end
  else if not (Abox.Checked) and Bbox.Checked and not (Cbox.Checked) and
    Dbox.Checked and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := B + D + E;
  end
  else if Abox.Checked and Bbox.Checked and not (Cbox.Checked) and
    Dbox.Checked and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := A + B + D + E;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and Cbox.Checked and
    Dbox.Checked and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := C + D + E;
  end
  else if Abox.Checked and not (Bbox.Checked) and Cbox.Checked and
    Dbox.Checked and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := A + C + D + E;
  end
  else if not (Abox.Checked) and Bbox.Checked and Cbox.Checked and
    Dbox.Checked and Ebox.Checked and not (Fbox.Checked) then
  begin
    str := B + C + D + E;
  end
  else if Abox.Checked and Bbox.Checked and Cbox.Checked and Dbox.Checked and
    Ebox.Checked and not (Fbox.Checked) then
  begin
    str := A + B + C + D + E;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and not (Cbox.Checked) and
    not (Dbox.Checked) and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := F;
  end
  else if Abox.Checked and not (Bbox.Checked) and not (Cbox.Checked) and
    not (Dbox.Checked) and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := A + F;
  end
  else if not (Abox.Checked) and Bbox.Checked and not (Cbox.Checked) and
    not (Dbox.Checked) and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := B + F;
  end
  else if Abox.Checked and Bbox.Checked and not (Cbox.Checked) and
    not (Dbox.Checked) and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := A + B + F;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and Cbox.Checked and
    not (Dbox.Checked) and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := C + F;
  end
  else if Abox.Checked and not (Bbox.Checked) and Cbox.Checked and
    not (Dbox.Checked) and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := A + C + F;
  end
  else if not (Abox.Checked) and Bbox.Checked and Cbox.Checked and
    not (Dbox.Checked) and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := B + C + F;
  end
  else if Abox.Checked and Bbox.Checked and Cbox.Checked and not
    (Dbox.Checked) and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := A + B + C + F;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and not (Cbox.Checked) and
    Dbox.Checked and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := D + F;
  end
  else if Abox.Checked and not (Bbox.Checked) and not (Cbox.Checked) and
    Dbox.Checked and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := A + D + F;
  end
  else if not (Abox.Checked) and Bbox.Checked and not (Cbox.Checked) and
    Dbox.Checked and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := B + D + F;
  end
  else if Abox.Checked and Bbox.Checked and not (Cbox.Checked) and
    Dbox.Checked and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := A + B + D + F;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and Cbox.Checked and
    Dbox.Checked and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := C + D + F;
  end
  else if Abox.Checked and not (Bbox.Checked) and Cbox.Checked and
    Dbox.Checked and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := A + C + D + F;
  end
  else if not (Abox.Checked) and Bbox.Checked and Cbox.Checked and
    Dbox.Checked and not (Ebox.Checked) and Fbox.Checked then
  begin
    str := B + C + D + F;
  end
  else if Abox.Checked and Bbox.Checked and Cbox.Checked and Dbox.Checked and
    not (Ebox.Checked) and Fbox.Checked then
  begin
    str := A + B + C + D + F;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and not (Cbox.Checked) and
    not (Dbox.Checked) and Ebox.Checked and Fbox.Checked then
  begin
    str := E + F;
  end
  else if Abox.Checked and not (Bbox.Checked) and not (Cbox.Checked) and
    not (Dbox.Checked) and Ebox.Checked and Fbox.Checked then
  begin
    str := A + E + F;
  end
  else if not (Abox.Checked) and Bbox.Checked and not (Cbox.Checked) and
    not (Dbox.Checked) and Ebox.Checked and Fbox.Checked then
  begin
    str := B + E + F;
  end
  else if Abox.Checked and Bbox.Checked and not (Cbox.Checked) and
    not (Dbox.Checked) and Ebox.Checked and Fbox.Checked then
  begin
    str := A + B + E + F;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and Cbox.Checked and
    not (Dbox.Checked) and Ebox.Checked and Fbox.Checked then
  begin
    str := C + E + F;
  end
  else if Abox.Checked and not (Bbox.Checked) and Cbox.Checked and
    not (Dbox.Checked) and Ebox.Checked and Fbox.Checked then
  begin
    str := A + C + E + F;
  end
  else if not (Abox.Checked) and Bbox.Checked and Cbox.Checked and
    not (Dbox.Checked) and Ebox.Checked and Fbox.Checked then
  begin
    str := B + C + E + F;
  end
  else if Abox.Checked and Bbox.Checked and Cbox.Checked and not
    (Dbox.Checked) and Ebox.Checked and Fbox.Checked then
  begin
    str := A + B + C + E + F;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and not (Cbox.Checked) and
    Dbox.Checked and Ebox.Checked and Fbox.Checked then
  begin
    str := D + E + F;
  end
  else if Abox.Checked and not (Bbox.Checked) and not (Cbox.Checked) and
    Dbox.Checked and Ebox.Checked and Fbox.Checked then
  begin
    str := A + D + E + F;
  end
  else if not (Abox.Checked) and Bbox.Checked and not (Cbox.Checked) and
    Dbox.Checked and Ebox.Checked and Fbox.Checked then
  begin
    str := B + D + E + F;
  end
  else if Abox.Checked and Bbox.Checked and not (Cbox.Checked) and
    Dbox.Checked and Ebox.Checked and Fbox.Checked then
  begin
    str := A + B + D + E + F;
  end
  else if not (Abox.Checked) and not (Bbox.Checked) and Cbox.Checked and
    Dbox.Checked and Ebox.Checked and Fbox.Checked then
  begin
    str := C + D + E + F;
  end
  else if Abox.Checked and not (Bbox.Checked) and Cbox.Checked and
    Dbox.Checked and Ebox.Checked and Fbox.Checked then
  begin
    str := A + C + D + E + F;
  end
  else if not (Abox.Checked) and Bbox.Checked and Cbox.Checked and
    Dbox.Checked and Ebox.Checked and Fbox.Checked then
  begin
    str := B + C + D + E + F;
  end
  else if Abox.Checked and Bbox.Checked and Cbox.Checked and Dbox.Checked and
    Ebox.Checked and Fbox.Checked then
  begin
    str := A + B + C + D + E + F;
  end
  else
  begin
    ShowMessage(SelectPWcharacters);
    str := '*';
  end;
  PasswordNew.Text := RandomPassword(Chars); // Generate password function
  PasswordStrengthMeter;
end;

function ListOfCharacterFrequencies(CFstr, CFustr: string): CardinalArray;
var
  u, s, p, o: cardinal;
begin
  SetLength(ListOfCharacterFrequencies, Length(CFustr) + 1);
  p := 0;
  for u := 1 to length(CFustr) do
    for s := 1 to length(CFstr) do
    begin
      o := p;
      p := PosEx(CFustr[u], CFstr, s);
      if (p > o) then
        Inc(ListOfCharacterFrequencies[u]);
    end;
end;

function entropy(s: string): extended;
var
  pf: CardinalArray;
  us: string;
  i, l: cardinal;
begin
  us := CountNumberOfUniqueCharacters(s);
  pf := ListOfCharacterFrequencies(s, us);
  l := length(s);
  entropy := 0.0;
  for i := 1 to length(us) do
  begin
    if pf[i] = 0 then
      pf[i] := 1; // Avoid Divide by Zero errors
    entropy -= pf[i] / l * log2(pf[i] / l);
  end;
end;

(* Password strength is a guide only. It doesn't use a dictionary but uses a version of shannon entropy *)
procedure TPasswordGenerator.PasswordStrengthMeter;
var
  PWlength, PWstrength: integer;
  NewPassword: string;
  SEscore: longint;
begin
  PWlength := SpinEdit1.Value;
  NewPassword := PasswordNew.Text;
  EntropyResult := entropy(NewPassword);
  SEscore := trunc(EntropyResult);
  PWstrength := SEscore * PWlength;
  (* display password strength *)

  if PWstrength <= 30 then
  begin
    StrengthIndicator.Brush.Color := $5353FF;
    lblPWStrength.Caption := VeryWeakPW;
  end
  else if (PWStrength >= 31) and (PWstrength <= 59) then
  begin
    StrengthIndicator.Brush.Color := $5353FF;
    lblPWStrength.Caption := WeakPW;
  end
  else if (PWStrength >= 60) and (PWstrength <= 160) then
  begin
    StrengthIndicator.Brush.Color := $54D0FA;
    lblPWStrength.Caption := AveragePW;
  end
  else if (PWStrength >= 161) and (PWstrength <= 300) then
  begin
    StrengthIndicator.Brush.Color := $F4C993;
    lblPWStrength.Caption := StrongPW;
  end
  else if PWStrength > 300 then
  begin
    StrengthIndicator.Brush.Color := $6CFFB6;
    lblPWStrength.Caption := VeryStrongPW;
  end;
end;

procedure TPasswordGenerator.TrayIcon1Click(Sender: TObject);
begin
  mainLazLock.WindowState := wsNormal;
  mainLazLock.Show;
end;

procedure TPasswordGenerator.btnGenPWMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := GenerateRandomPW;
end;

procedure TPasswordGenerator.CboxMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := STATUSenableUpperCase;
end;

procedure TPasswordGenerator.DboxMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := STATUSenableLowerCase;
end;

procedure TPasswordGenerator.EboxMouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := STATUSenableBrackets;
end;

procedure TPasswordGenerator.ClearStatusBar(Sender: TObject);
begin
  StatusBar.SimpleText := '';
end;

procedure TPasswordGenerator.SpinEdit1MouseEnter(Sender: TObject);
begin
  StatusBar.SimpleText := STATUSselectPWlength;
end;

(*  Edit Entry tab  *)

procedure TPasswordGenerator.edCategoryChange(Sender: TObject);
begin
  case edCategory2.ItemIndex of
    0: enteredCategory := 'Banking';
    1: enteredCategory := 'Education';
    2: enteredCategory := 'Email';
    3: enteredCategory := 'Media';
    4: enteredCategory := 'Other';
    5: enteredCategory := 'Shopping';
    6: enteredCategory := 'Social Media';
    7: enteredCategory := 'Software';
    8: enteredCategory := 'Web';
    9: enteredCategory := 'Work';
  end;
end;

procedure TPasswordGenerator.edImage2Change(Sender: TObject);
begin
  case edImage2.ItemIndex of
    0: image := '0';
    1: image := '1';
    2: image := '2';
    3: image := '3';
    4: image := '4';
    5: image := '5';
    6: image := '6';
    7: image := '7';
    8: image := '8';
    9: image := '9';
    10: image := '10';
    11: image := '11';
    12: image := '12';
    13: image := '13';
    14: image := '14';
    15: image := '15';
    16: image := '16';
    17: image := '17';
    18: image := '18';
    19: image := '19';
    20: image := '20';
    21: image := '21';
    22: image := '22';
    23: image := '23';
    24: image := '24';
    25: image := '25';
    26: image := '26';
    27: image := '27';
    28: image := '28';
    29: image := '29';
    30: image := '30';
    31: image := '31';
    32: image := '32';
    33: image := '33';
    34: image := '34';
    35: image := '35';
    36: image := '36';
    37: image := '37';
    38: image := '38';
    39: image := '39';
  end;
end;

procedure TPasswordGenerator.edImage2DrawItem(Control: TWinControl;
  Index: integer; ARect: TRect; State: TOwnerDrawState);
begin
  edImage2.Canvas.FillRect(ARect);
  edImage2.Canvas.TextRect(ARect, 25, ARect.Top, edImage2.Items[Index]);
  mainLazLock.sitesImageList.Draw(edImage2.Canvas, ARect.Left + 1,
    ARect.Top + 1, Index);
end;

procedure TPasswordGenerator.ClearAndClose;
begin
  edNameA.Text := '';
  edUserA.Text := '';
  edURLA.Text := '';
  edPasswordA.Text := '';
  edImage.ItemIndex := -1;
  edCategory.ItemIndex := -1;
  Close;
end;

procedure TPasswordGenerator.btnSaveChangesClick(Sender: TObject);
var
  i2: integer;
begin
  // Check that all fields are completed
  if (edCategory2.ItemIndex <> -1) and (edNameB.Text <> '') and
    (edURLB.Text <> '') and (edUserB.Text <> '') and (edPasswordB.Text <> '') and
    (edImage2.ItemIndex <> -1) then
  begin
    //delete existing entry then create a new one
    mainLazLock.ListView1.Items.BeginUpdate;
    for i2 := mainLazLock.ListView1.Items.Count - 1 downto 0 do
      if mainLazLock.ListView1.Items[i2].Selected then
        mainLazLock.ListView1.Items.Delete(i2);
    mainLazLock.ListView1.Items.EndUpdate;
    enteredCategory := edCategory2.Text;
    enteredName := edNameB.Text;
    enteredURL := edURLB.Text;
    enteredUser := edUserB.Text;
    enteredPassword := edPasswordB.Text;
    image := IntToStr(edImage2.ItemIndex);
    with mainLazLock.ListView1.Items.Add do
    begin
      Caption := enteredCategory;
      SubItems.Add(enteredName);
      SubItems.Add(enteredURL);
      SubItems.Add(enteredUser);
      SubItems.Add(enteredPassword);
      SubItems.Add(image);
      unitmain.EditedYes := True;
      unitmain.IsSaved := False;
      mainLazLock.Caption := 'LazLock *';
    end;
    Close;
  end
  else
    ShowMessage(CompleteAllFields);
end;

{ TPasswordGenerator }

end.
