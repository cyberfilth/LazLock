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

unit unitmain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ECBevel, ECEditBtns, DCPrijndael, DCPsha256,
  usplashabout, Forms, Controls, Dialogs, Menus, ExtCtrls, ComCtrls, Buttons,
  StdCtrls, Clipbrd, LCLIntf, DefaultTranslator, ListViewFilterEdit, EditBtn,
  XMLPropStorage;

type

  { TmainLazLock }

  TmainLazLock = class(TForm)
    btnEducation: TECSpeedBtn;
    btnEmail: TECSpeedBtn;
    btnMedia: TECSpeedBtn;
    btnOther: TECSpeedBtn;
    btnShopping: TECSpeedBtn;
    btnSocialMedia: TECSpeedBtn;
    btnSoftware: TECSpeedBtn;
    btnWeb: TECSpeedBtn;
    btnWork: TECSpeedBtn;
    categoryImageList: TImageList;
    DCP_rijndael1: TDCP_rijndael;
    DCP_sha256_1: TDCP_sha256;
    btnShowAll: TECSpeedBtn;
    btnBanking: TECSpeedBtn;
    ListViewFilterEdit1: TListViewFilterEdit;
    MenuItem1: TMenuItem;
    RCMmnuCopyPW: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem6: TMenuItem;
    RCMmnuCopyURL: TMenuItem;
    RCMmnuCopyUser: TMenuItem;
    STMmnuExit: TMenuItem;
    STMmnuPWGen: TMenuItem;
    STMmnuAddEntry: TMenuItem;
    SysTrayMenu: TPopupMenu;
    RCMmnuOpenURL: TMenuItem;
    MenuItem7: TMenuItem;
    RCmnuOpenUrl: TMenuItem;
    mnuAbout: TMenuItem;
    mnuOnlineHelp: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem5: TMenuItem;
    mnuExportText: TMenuItem;
    mnuChangePW: TMenuItem;
    CategoryPanel: TPanel;
    CategoryHeader: TPanel;
    RCMmnuAddEntry: TMenuItem;
    RCMmnuPWGen: TMenuItem;
    RightClickMini: TPopupMenu;
    RCmnuPWGenerator: TMenuItem;
    RCmnuCopyUsername: TMenuItem;
    RCmnuCopyPassword: TMenuItem;
    RCmnuCopyURL: TMenuItem;
    RCmnuAddEntry: TMenuItem;
    RCmnuEditEntry: TMenuItem;
    RCmnuDeleteEntry: TMenuItem;
    mnuBackup: TMenuItem;
    mnuSeparator: TMenuItem;
    mnuDelete: TMenuItem;
    mnuPWGenerator: TMenuItem;
    mnuEdit: TMenuItem;
    mnuAdd: TMenuItem;
    mnuHelp: TMenuItem;
    mnuSave: TMenuItem;
    mnuExit: TMenuItem;
    RightClickMenu: TPopupMenu;
    SpacerPanel: TPanel;
    SaveDialog1: TSaveDialog;
    sitesImageList: TImageList;
    copyPassword: TSpeedButton;
    copyURL: TSpeedButton;
    ECBevel1: TECBevel;
    LabelURL: TLabel;
    LabelUser: TLabel;
    LabelPassword: TLabel;
    ResultURL: TEdit;
    ResultImage: TImage;
    ListView1: TListView;
    mnuFile: TMenuItem;
    mnuEditDropdown: TMenuItem;
    mnuImageList: TImageList;
    MainMenu1: TMainMenu;
    Panel1: TPanel;
    ResultUser: TEdit;
    ResultPassword: TEdit;
    copyUser: TSpeedButton;
    GoToURL: TSpeedButton;
    ResultName: TStaticText;
    SplashAbout1: TSplashAbout;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    TBAddEntry: TToolButton;
    TBRemove: TToolButton;
    TBEditEntry: TToolButton;
    TBdivider2: TToolButton;
    TBPassword: TToolButton;
    TBdivider1: TToolButton;
    TBdivider3: TToolButton;
    TBQuit: TToolButton;
    TBSave: TToolButton;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnBankingClick(Sender: TObject);
    procedure btnEducationClick(Sender: TObject);
    procedure btnEmailClick(Sender: TObject);
    procedure btnMediaClick(Sender: TObject);
    procedure btnShoppingClick(Sender: TObject);
    procedure btnOtherClick(Sender: TObject);
    procedure btnShowAllClick(Sender: TObject);
    procedure btnSocialMediaClick(Sender: TObject);
    procedure btnSoftwareClick(Sender: TObject);
    procedure btnWebClick(Sender: TObject);
    procedure btnWorkClick(Sender: TObject);
    procedure copyPasswordClick(Sender: TObject);
    procedure copyURLClick(Sender: TObject);
    procedure copyURLMouseEnter(Sender: TObject);
    procedure copyUserClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormWindowStateChange(Sender: TObject);
    procedure GoToURLClick(Sender: TObject);
    procedure GoToURLMouseEnter(Sender: TObject);
    procedure CopyMessage;
    procedure ClearStatusMessage;
    procedure EncryptAndSave;
    procedure DecryptAndOpen;
    procedure GetPassword;
    procedure CheckPassword;
    procedure ExitDecryption;
    procedure CategoryViewClicked;
    procedure ListView1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ListView1SelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure ListViewFilterEdit1Change(Sender: TObject);
    procedure LoadFile;
    procedure HashPassword;
    procedure ListViewClicked;
    procedure mnuHelpClick(Sender: TObject);
    procedure mnuChangePWClick(Sender: TObject);
    procedure mnuOnlineHelpClick(Sender: TObject);
    procedure NewVault;
    procedure ListView1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure SavePrompt;
    procedure mnuExportClick(Sender: TObject);
    procedure TBAddEntryClick(Sender: TObject);
    procedure TBAddEntryMouseEnter(Sender: TObject);
    procedure TBEditEntryClick(Sender: TObject);
    procedure TBEditEntryMouseEnter(Sender: TObject);
    procedure TBPasswordClick(Sender: TObject);
    procedure TBPasswordMouseEnter(Sender: TObject);
    procedure TBQuitClick(Sender: TObject);
    procedure TBQuitMouseEnter(Sender: TObject);
    procedure TBRemoveClick(Sender: TObject);
    procedure TBRemoveMouseEnter(Sender: TObject);
    procedure TBSaveClick(Sender: TObject);
    procedure TBSaveMouseEnter(Sender: TObject);
    procedure TBSaveMouseLeave(Sender: TObject);
    procedure WriteToStringList;
    procedure WriteToCategoryStringList;
    procedure RemoveBanking;
    procedure RemoveEducation;
    procedure RemoveEmail;
    procedure RemoveMedia;
    procedure RemoveOther;
    procedure RemoveShopping;
    procedure RemoveSocialMedia;
    procedure RemoveSoftware;
    procedure RemoveWeb;
    procedure RemoveWork;
    procedure ShowAll;
  private
    { private declarations }
  public
    { public declarations }
    BackupListView: TStringList;
  end;

const
  {$IFDEF Windows}
  SavedData = 'LLdata\lazlock.vault';
  {$ENDIF}
  {$IFDEF Linux}
  SavedData = 'LLdata/lazlock.vault';
  {$ENDIF}

var
  mainLazLock: TmainLazLock;
  IsSaved, IsNew, FullShutdown, IsStringListSaved, EditedYes: boolean;
  BackupListView, DeleteStringList: TStringList;
  PasswordKey, HashedPassKey, CategorySelection: string;
  vaultStringStream: TStringStream;
  iCF, xCF: integer;
  FilterList: TListItem;
  S: TResourceStream;
  LVItemsCount: integer;

implementation

uses
  unitpassword, unittranslate;

{$R *.lfm}

{ TmainLazLock }

(*  Create form and check if password file exists  *)
procedure TmainLazLock.FormCreate(Sender: TObject);
begin
  BackupListView := TStringList.Create;
  DeleteStringList := TStringList.Create;
  FullShutdown := False;
  EditedYes := False;
  if FileExists(SavedData) then
    GetPassword
  else
    NewVault; // Create new file if one doesn't exist
end;

procedure TmainLazLock.FormWindowStateChange(Sender: TObject);
// Hide in systray when minimised
begin
  if WindowState = wsMinimized then
  begin
    WindowState := wsMinimized;
    Hide;
  end;
end;

procedure TmainLazLock.HashPassword;
var
  NewHash: TDCP_sha256;
  NewDigest: array[0..31] of byte;
  i: integer;
begin
  NewHash := TDCP_sha256.Create(Self); // Create a hash of the entered password
  NewHash.Init;
  NewHash.UpdateStr(PasswordKey);
  NewHash.Final(NewDigest);
  HashedPassKey := '';
  for i := 0 to 31 do
    HashedPassKey := HashedPassKey + IntToHex(NewDigest[i], 2);
end;

procedure TmainLazLock.NewVault;
var
  newInput1, newInput2: string;

begin
  newInput1 := '';
  newInput2 := '';
  if InputQuery(CreateCaption, NewVaultMessage, True, newInput1) then
  // Prompt user to confirm password
  else // If user clicked Cancel or entered nothing, LazLock will quit
  begin
    ShowMessage(NoPasswordExit);
    Halt;
  end;
  while newInput2 = '' do
    InputQuery(ConfirmCaption, ReenterPW, True, newInput2);
  if newInput1 <> newInput2 then
  begin
    ShowMessage(DontMatch);
    FullShutdown := True;
    Application.Terminate;
  end;
  PasswordKey := newInput1;
  CreateDir('LLdata');
  IsSaved := False;
  IsNew := True;
  (* create hash of password *)
  HashPassword;
end;

procedure TmainLazLock.GetPassword; // Prompt for password
begin
  PasswordKey := ''; // reset password
  InputQuery(EnterPWCaption, EnterPW, True, PasswordKey);
  DecryptAndOpen;
end;

procedure TmainLazLock.DecryptAndOpen;
var
  vaultFileData: TFileStream;
  DecryptionAlgorithm: TDCP_rijndael;
begin
  try
    vaultStringStream := TStringStream.Create('');
    vaultFileData := TFileStream.Create(SavedData, fmOpenRead);
    DecryptionAlgorithm := TDCP_rijndael.Create(nil);
    DecryptionAlgorithm.InitStr(PasswordKey, TDCP_sha256);
    DecryptionAlgorithm.DecryptStream(vaultFileData, vaultStringStream,
      vaultFileData.Size);
    DecryptionAlgorithm.Burn;
  finally
    FreeAndNil(vaultFileData);
    FreeAndNil(DecryptionAlgorithm);
  end;
  CheckPassword;
end;

procedure TmainLazLock.CheckPassword;
var
  FirstLine: string;
begin
  HashPassword;
  vaultStringStream.Position := 0;
  FirstLine := vaultStringStream.DataString;
  FirstLine := LeftStr(FirstLine, 64); // get stored password hash from file
  if HashedPassKey = FirstLine then // compare the passwords
  begin
    FirstLine := '';
    LoadFile;
  end
  else
    ExitDecryption;
end;

procedure TmainLazLock.ExitDecryption;
begin
  ShowMessage(ExitDecryptMessage);
  FreeAndNil(vaultStringStream);
  Application.Terminate;
end;

procedure TmainLazLock.ListView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ListViewClicked;
end;

procedure TmainLazLock.ListViewClicked;
var
  image: integer;
begin
  if ListView1.Selected <> nil then // Avoids error when a blank line is selected
  begin
    ResultName.Caption := ListView1.selected.SubItems[0];
    ResultURL.Text := ListView1.selected.SubItems[1];
    ResultUser.Text := ListView1.selected.SubItems[2];
    ResultPassword.Text := ListView1.selected.SubItems[3];
    image := StrToInt(ListView1.selected.SubItems[4]);
    sitesImageList.GetBitmap(image, ResultImage.Picture.Bitmap);
  end;
  (* This code disables edit functions when using the search box *)
  if ListViewFilterEdit1.Text = '' then
  begin
    TBEditEntry.Enabled := True; // Allows selected entry to be edited
    TBRemove.Enabled := True; // or deleted
    mnuEdit.Enabled := True;  // Dropdown menu options
    mnuDelete.Enabled := True;  // remove option disabled
    ListView1.PopupMenu := RightClickMenu; // enable full Right-Click menu
  end
  else
  begin
    TBEditEntry.Enabled := False; // Allows selected entry to be edited
    TBRemove.Enabled := False; // or deleted
    mnuEdit.Enabled := False;  // Dropdown menu options
    mnuDelete.Enabled := False;  // remove option enabled
    ListView1.PopupMenu := RightClickMini; // enable mini Right-Click menu
  end;
end;

procedure TmainLazLock.ListView1SelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  ListViewClicked;
end;

procedure TmainLazLock.ListViewFilterEdit1Change(Sender: TObject);
begin
  mainLazLock.TBEditEntry.Enabled := False; // Edit button greyed out
  mainLazLock.TBRemove.Enabled := False; // Remove button greyed out
  mainLazLock.mnuEdit.Enabled := False;  // Dropdown menu options
  mainLazLock.mnuDelete.Enabled := False; //Delete button greyed out
  ListView1.PopupMenu := RightClickMini; // enable mini Right-Click menu
  ShowAll;
  //  WriteToStringList;
  ListView1.AlphaSort;
  ListViewFilterEdit1.FilteredListview := ListView1;
end;

procedure TmainLazLock.LoadFile;
var
  Line: string;
  index, comPos, I: integer;
  item: TListItem;
  vaultStringList: TStringList;
begin
  ListView1.Clear;
  vaultStringStream.Position := 64; // Start reading file after hashed password
  vaultStringList := TStringList.Create;
  vaultStringList.LoadFromStream(vaultStringStream);
  (* Remove any empty entries from the list *)
  for I := vaultStringList.Count - 1 downto 0 do
  begin
    if Trim(vaultStringList[I]) = '' then
      vaultStringList.Delete(I);
  end;
  vaultStringList.Sort;  // Organise list into alphabetical order
  (*  Fill ListView with entries  *)
  for index := 0 to vaultStringList.Count - 1 do
  begin
    Line := vaultStringList[index];
    item := ListView1.Items.Add;
    comPos := Pos(#9, Line);
    item.Caption := Copy(Line, 1, comPos - 1);
    Delete(Line, 1, comPos);
    comPos := Pos(#9, Line);
    while comPos > 0 do
    begin
      item.SubItems.Add(Copy(Line, 1, comPos - 1));
      Delete(Line, 1, comPos);
      comPos := Pos(#9, Line);
    end;
    item.SubItems.Add(Line);
  end;
  vaultStringList.Free;
  vaultStringStream.Free;
  mainLazLock.TBEditEntry.Enabled := False; // Edit button greyed out
  mainLazLock.TBRemove.Enabled := False; // Remove button greyed out
  mainLazLock.mnuEdit.Enabled := False;  // Dropdown menu options
  mainLazLock.mnuDelete.Enabled := False; //Delete button greyed out
  IsSaved := True;
end;

procedure TmainLazLock.mnuHelpClick(Sender: TObject);
begin
  SplashAbout1.ShowAbout;
end;

procedure TmainLazLock.mnuChangePWClick(Sender: TObject);
var
  newPassword1, newPassword2: string;
begin
  newPassword1 := '';
  newPassword2 := '';
  while newPassword1 = '' do
    InputQuery(ChangeCaption, NewPW1Message, True, newPassword1);
  while newPassword2 = '' do
    InputQuery(ConfirmCaption, ReenterPW, True, newPassword2);
  if newPassword1 <> newPassword2 then
  begin
    ShowMessage(NoChangeMessage);
  end
  else
  begin
    PasswordKey := newPassword1;
    HashPassword;
    EncryptAndSave;
  end;
end;

procedure TmainLazLock.mnuOnlineHelpClick(Sender: TObject);
begin
  OpenURL('https://www.cpunk-security.com/lazlock2.html');
end;

procedure TmainLazLock.GoToURLClick(Sender: TObject);
var
  siteAddress: string;
begin
  siteAddress := ResultURL.Text;
  OpenURL(siteAddress);
end;

procedure TmainLazLock.GoToURLMouseEnter(Sender: TObject);
begin
  StatusBar1.SimpleText := OpenLinkDefaultBrowser;
end;

procedure TmainLazLock.CopyMessage;
begin
  StatusBar1.SimpleText := CopyToCBoard;
end;

procedure TmainLazLock.ClearStatusMessage;
begin
  StatusBar1.SimpleText := IntToStr(LVItemsCount) + STATUStotalRecords;
end;

function FormatString(s1, s2, s3, s4, s5: string): string;
var
  s: string;
  I: integer;
begin
  Result := '';
  for I := 1 to 5 do
  begin
    s := '';
    case I of
      1: s := s1;
      2: s := s2;
      3: s := s3;
      4: s := s4;
      5: s := s5;
    end;
    if Length(s) < 100 then
      repeat
        s := s + ' ';
      until Length(s) = 100;
    Result := Result + s;
  end;
end;

procedure TmainLazLock.mnuExportClick(Sender: TObject);
var
  I: integer;
  SL: TStringList;
begin
  ShowAll;
  if SaveDialog1.Execute then
  begin
    SL := TStringList.Create;
    try
      SL.Add(FormatString('Category', 'Site', 'URL', 'Username', 'Password'));
      SL.Add(FormatString('--------', '----', '---', '--------', '--------'));
      for I := 0 to ListView1.Items.Count - 1 do
        SL.Add(FormatString(ListView1.Items[I].Caption,
          ListView1.Items[I].SubItems[0], ListView1.Items[I].SubItems[1],
          ListView1.Items[I].SubItems[2], ListView1.Items[I].SubItems[3]));
      SL.SaveToFile(SaveDialog1.FileName);
    finally
      SL.Free;
    end;
  end;
end;

procedure TmainLazLock.TBAddEntryClick(Sender: TObject);
begin
  // Detach the Search Bar from the ListView
  ListViewFilterEdit1.FilteredListview := nil;
  ListViewFilterEdit1.Items.Clear;
  ListViewFilterEdit1.Clear;
  ListViewFilterEdit1.Text := '';
  ShowAll;
  PasswordGenerator.PageControl1.ActivePage := PasswordGenerator.tabEnterNew;
  PasswordGenerator.tabEnterNew.TabVisible := True;
  PasswordGenerator.tabEditEntry.TabVisible := False;
  PasswordGenerator.tabPWGenerator.TabVisible := True;
  PasswordGenerator.Caption := AddEntryPWGeneratorCaption;
  PasswordGenerator.edCategory.Caption := CategorySelect;
  // Change caption on modal form
  PasswordGenerator.ShowModal;
  ListView1.AlphaSort;
  // Disable Edit & Delete options to prevent runtime error 210 when ListView1 doesn't have focus
  TBEditEntry.Enabled := False;
  mnuEdit.Enabled := False;
  TBRemove.Enabled := False;
  mnuDelete.Enabled := False;
  // Clear all details fields
  ResultName.Caption := '';
  ResultUser.Text := '';
  ResultURL.Text := '';
  ResultPassword.Text := '';
  ResultImage.Picture := nil;
  BackupListView.Clear;
  WriteToStringList;
  // Re-attach search box
  ListViewFilterEdit1.FilteredListview := ListView1;
  IsSaved := False;
  mainLazLock.Caption := 'LazLock *';
  // Update counter on footer
  LVItemsCount := ListView1.Items.Count;
  ClearStatusMessage;
end;

procedure TmainLazLock.TBAddEntryMouseEnter(Sender: TObject);
begin
  StatusBar1.SimpleText := STATUSaddEntry;
end;

procedure TmainLazLock.TBEditEntryClick(Sender: TObject);
var
  DELcategory, DELname, DELurl, DELuser, DELpassword: string;
  Xselect: integer;
begin
  if (ListView1.Selected <> nil) and (ListViewFilterEdit1.Text = '') then // Check an entry has been selected first and search bar is empty
  begin
    // Detach the Search Bar from the ListView
    ListViewFilterEdit1.FilteredListview := nil;
    ListViewFilterEdit1.Items.Clear;
    ListViewFilterEdit1.Clear;
    // Store original values for later deletion
    DELcategory := ListView1.selected.Caption;
    DELname := ResultName.Caption;
    DELurl := ResultURL.Text;
    DELuser := ResultUser.Text;
    DELpassword := ResultPassword.Text;
    // Set up Edit Form controls
    PasswordGenerator.PageControl1.ActivePage := PasswordGenerator.tabEditEntry;
    PasswordGenerator.tabEditEntry.TabVisible := True; // enable tabs
    PasswordGenerator.tabPWGenerator.TabVisible := True;
    PasswordGenerator.tabEnterNew.TabVisible := False;
    // Fill the edit boxes with the original values
    PasswordGenerator.edNameB.Text := ResultName.Caption;
    PasswordGenerator.edURLB.Text := ResultURL.Text;
    PasswordGenerator.edUserB.Text := ResultUser.Text;
    PasswordGenerator.edPasswordB.Text := ResultPassword.Text;
    PasswordGenerator.edCategory2.Text := ListView1.selected.Caption;
    PasswordGenerator.edImage2.ItemIndex := StrToInt(ListView1.Selected.SubItems[4]);
    // Change caption of modal form
    PasswordGenerator.Caption := EditEntryPWGeneratorCaption;
    ShowAll;
    // Assign labels to text boxes
    PasswordGenerator.ShowModal;
    // Find the original entry
    if EditedYes = True then
    begin
      try
        with ListView1 do ;
        for Xselect := 0 to listview1.Items.Count - 1 do
          if (SameText(ListView1.Items[Xselect].Caption, DELcategory)) and
            (SameText(ListView1.Items[Xselect].subitems[0], DELname)) and
            (SameText(ListView1.Items[Xselect].subitems[1], DELurl)) and
            (SameText(ListView1.Items[Xselect].subitems[2], DELuser)) and
            (SameText(ListView1.Items[Xselect].subitems[3], DELpassword)) then
            ListView1.Selected := ListView1.Items[Xselect]; // Select original entry
        ListView1.Selected.Delete; // Delete the original entry
      except
        // Access violation is expected here due to counter running down
        on E: Exception do
          ShowMessage('LazLock Error: ' + e.Message);
      end;
      EditedYes := False;
    end;
    Xselect := 0;
    DELcategory := '';
    DELname := '';
    DELurl := '';
    DELuser := '';
    DELpassword := '';
    ListView1.AlphaSort; // Allow list to be sorted
    WriteToStringList;
    IsStringListSaved := False;
    // Disable Edit & Delete options to prevent runtime error 210 when ListView1 doesn't have focus
    TBEditEntry.Enabled := False;
    mnuEdit.Enabled := False;
    TBRemove.Enabled := False;
    mnuDelete.Enabled := False;
  end
  else
    ShowMessage(NoEntryMessage);
end;

procedure TmainLazLock.TBEditEntryMouseEnter(Sender: TObject);
begin
  StatusBar1.SimpleText := STATUSeditEntry;
end;

procedure TmainLazLock.TBPasswordClick(Sender: TObject);
begin
  PasswordGenerator.PageControl1.ActivePage := PasswordGenerator.tabPWGenerator;
  PasswordGenerator.tabEnterNew.TabVisible := False;
  PasswordGenerator.tabEditEntry.TabVisible := False;
  PasswordGenerator.Caption := PWgeneratorCaption; // change caption on modal form
  PasswordGenerator.ShowModal;
end;

procedure TmainLazLock.TBPasswordMouseEnter(Sender: TObject);
begin
  StatusBar1.SimpleText := OpenPWgeneratorCaption;
end;

procedure TmainLazLock.TBQuitClick(Sender: TObject);
begin
  if IsSaved = False then
    SavePrompt;
  HashedPasskey := '';
  FreeAndNil(BackupListView);
  FreeAndNil(DeleteStringList);
  // Clear Clipboard
  Clipboard.AsText := '';
  // Clear all details fields
  ResultName.Caption := '';
  ResultUser.Text := '';
  ResultURL.Text := '';
  ResultPassword.Text := '';
  ResultImage.Picture := nil;
  Application.Terminate;
end;

procedure TmainLazLock.TBQuitMouseEnter(Sender: TObject);
begin
  StatusBar1.SimpleText := STATUSexitLazLock;
end;

procedure TmainLazLock.WriteToCategoryStringList;
// Backup all entries in selected Category

  procedure AddTextToLine(var Line: string; const Text: string);
  begin
    Line := Line + Text + #9;
  end;

  procedure MoveCompletedLineToList(const Strings: TStringList; var Line: string);
  begin
    Strings.Add(System.Copy(Line, 1, Length(Line) - 1));//remove trailing tab
    Line := '';
  end;

var
  LatestLine: string;
  i2, j: integer;
begin
  // backup listview
  LatestLine := '';

  try
    for i2 := 0 to ListView1.Items.Count - 1 do
    begin
      AddTextToLine(LatestLine, ListView1.Items[i2].Caption);
      for j := 0 to ListView1.Items[i2].SubItems.Count - 1 do
      begin
        AddTextToLine(LatestLine, ListView1.Items[i2].SubItems[j]);
      end;
      MoveCompletedLineToList(DeleteStringList, LatestLine);
    end;
  except
    on E: Exception do
      ShowMessage('Error writing file: ' + e.Message);
  end;
end;

procedure TmainLazLock.TBRemoveClick(Sender: TObject);
var
  DELETEcategory, DELETEname, DELETEurl, DELETEuser, DELETEpassword: string;
  i: integer;
begin
  // Check an entry has been selected first
  if ResultName.Caption <> '' then
  begin
    // Confirm that the user wants to delete this entry
    if MessageDlg(MSGareYouSure + ResultName.Caption + '?', mtConfirmation,
      [mbYes, mbNo], 0) = mrNo then
    begin
      Exit;
    end;
    // Detach the Search Bar from the ListView
    ListViewFilterEdit1.FilteredListview := nil;
    ListViewFilterEdit1.Items.Clear;
    ListViewFilterEdit1.Clear;
    // Store the values to be deleted
    DELETEcategory := ListView1.selected.Caption;
    DELETEname := ResultName.Caption;
    DELETEurl := ResultURL.Text;
    DELETEuser := ResultUser.Text;
    DELETEpassword := ResultPassword.Text;
    ShowAll;
    // Delete entry
    begin
      try
        with ListView1 do ;
        for i := 0 to listview1.Items.Count - 1 do
          if (SameText(ListView1.Items[i].Caption, DELETEcategory)) and
            (SameText(ListView1.Items[i].subitems[0], DELETEname)) and
            (SameText(ListView1.Items[i].subitems[1], DELETEurl)) and
            (SameText(ListView1.Items[i].subitems[2], DELETEuser)) and
            (SameText(ListView1.Items[i].subitems[3], DELETEpassword)) then
            ListView1.Selected := ListView1.Items[i]; // Select original entry
        ListView1.Selected.Delete; // Delete the original entry
      except
        // Access violation is expected here due to counter running down
        on E: Exception do
          ShowMessage('LazLock Error: ' + e.Message);
      end;
    end;
    // Reset all values
    i := 0;
    DELETEcategory := '';
    DELETEname := '';
    DELETEurl := '';
    DELETEuser := '';
    DELETEpassword := '';
    ListView1.AlphaSort; // Allow list to be sorted
    // Disable Edit & Delete options to prevent runtime error 210 when ListView1 doesn't have focus
    TBEditEntry.Enabled := False;
    mnuEdit.Enabled := False;
    TBRemove.Enabled := False;
    mnuDelete.Enabled := False;
    // DeleteStringList.Clear;
    BackupListView.Clear;
    WriteToStringList;
    IsSaved := False;
    mainLazLock.Caption := 'LazLock *';
    // Update counter on footer
    LVItemsCount := ListView1.Items.Count;
    ClearStatusMessage;
  end
  else
    ShowMessage(NoEntryMessage);
end;

procedure TmainLazLock.TBRemoveMouseEnter(Sender: TObject);
begin
  StatusBar1.SimpleText := STATUSremoveEntry;
end;

procedure TmainLazLock.EncryptAndSave; // Encrypt & Save file

  procedure AddTextToLine(var Line: string; const Text: string);
  begin
    Line := Line + Text + #9;
  end;

  procedure MoveCompletedLineToList(const Strings: TStringList; var Line: string);
  begin
    Strings.Add(System.Copy(Line, 1, Length(Line) - 1));//remove trailing tab
    Line := '';
  end;

var
  Strings: TStringList;
  LatestLine: string;
  i, j: integer;
  // Encryption variables
  EncryptionAlgorithm: TDCP_rijndael;
  SaveData: TFileStream;
  StreamData: TStringStream;
begin
  LatestLine := '';
  Strings := TStringList.Create;
  try
    Strings.Add(HashedPassKey);
    for i := 0 to ListView1.Items.Count - 1 do
    begin
      AddTextToLine(LatestLine, ListView1.Items[i].Caption);
      for j := 0 to ListView1.Items[i].SubItems.Count - 1 do
      begin
        AddTextToLine(LatestLine, ListView1.Items[i].SubItems[j]);
      end;
      MoveCompletedLineToList(Strings, LatestLine);
    end;
    // Begin encryption
    SaveData := TFileStream.Create(SavedData, fmCreate);
    StreamData := TStringStream.Create('');
    Strings.SaveToStream(StreamData);
    StreamData.Position := 0;
    EncryptionAlgorithm := TDCP_rijndael.Create(nil);
  finally
  end;
  try
    EncryptionAlgorithm.InitStr(PasswordKey, TDCP_sha256);
    EncryptionAlgorithm.EncryptStream(StreamData, SaveData, StreamData.size);
    EncryptionAlgorithm.Burn;
  finally
    Strings.Free;
    FreeAndNil(EncryptionAlgorithm);
    FreeAndNil(StreamData);
    FreeAndNil(SaveData);
    IsSaved := True;
    mainLazLock.Caption := 'LazLock';
  end;
end;

procedure TmainLazLock.WriteToStringList; // Backup all entries into a stringlist

  procedure AddTextToLine(var Line: string; const Text: string);
  begin
    Line := Line + Text + #9;
  end;

  procedure MoveCompletedLineToList(const Strings: TStringList; var Line: string);
  begin
    Strings.Add(System.Copy(Line, 1, Length(Line) - 1));//remove trailing tab
    Line := '';
  end;

var
  LatestLine: string;
  i2, j: integer;
begin
  LatestLine := '';
  BackupListView.Clear;
  try
    for i2 := 0 to ListView1.Items.Count - 1 do
    begin
      AddTextToLine(LatestLine, ListView1.Items[i2].Caption);
      for j := 0 to ListView1.Items[i2].SubItems.Count - 1 do
      begin
        AddTextToLine(LatestLine, ListView1.Items[i2].SubItems[j]);
      end;
      MoveCompletedLineToList(BackupListView, LatestLine);
    end;
  except
    on E: Exception do
      ShowMessage('LazLock Error: ' + e.Message);
  end;
  BackupListView.Sort;
  IsStringListSaved := True;
end;

procedure TmainLazLock.CategoryViewClicked;
var
  image: integer;
begin
  //Clear the search box
  ListViewFilterEdit1.Text := '';
  // Hide all categories except the selected one
  if IsStringListSaved = False then
    WriteToStringList; // Check to see if categories saved
  if CategorySelection = 'All' then
  begin
    ShowAll;
  end
  else if CategorySelection = 'Banking' then
  begin
    ShowAll;
    RemoveEducation;
    RemoveEmail;
    RemoveMedia;
    RemoveOther;
    RemoveShopping;
    RemoveSocialMedia;
    RemoveSoftware;
    RemoveWeb;
    RemoveWork;
  end
  else if CategorySelection = 'Education' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEmail;
    RemoveMedia;
    RemoveOther;
    RemoveShopping;
    RemoveSocialMedia;
    RemoveSoftware;
    RemoveWeb;
    RemoveWork;
  end
  else if CategorySelection = 'Email' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEducation;
    RemoveMedia;
    RemoveOther;
    RemoveShopping;
    RemoveSocialMedia;
    RemoveSoftware;
    RemoveWeb;
    RemoveWork;
  end
  else if CategorySelection = 'Media' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEducation;
    RemoveEmail;
    RemoveOther;
    RemoveShopping;
    RemoveSocialMedia;
    RemoveSoftware;
    RemoveWeb;
    RemoveWork;
  end
  else if CategorySelection = 'Other' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEducation;
    RemoveEmail;
    RemoveMedia;
    RemoveShopping;
    RemoveSocialMedia;
    RemoveSoftware;
    RemoveWeb;
    RemoveWork;
  end
  else if CategorySelection = 'Shopping' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEducation;
    RemoveEmail;
    RemoveMedia;
    RemoveOther;
    RemoveSocialMedia;
    RemoveSoftware;
    RemoveWeb;
    RemoveWork;
  end
  else if CategorySelection = 'Social Media' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEducation;
    RemoveEmail;
    RemoveMedia;
    RemoveOther;
    RemoveShopping;
    RemoveSoftware;
    RemoveWeb;
    RemoveWork;
  end
  else if CategorySelection = 'Software' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEducation;
    RemoveEmail;
    RemoveMedia;
    RemoveOther;
    RemoveShopping;
    RemoveSocialMedia;
    RemoveWeb;
    RemoveWork;
  end
  else if CategorySelection = 'Web' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEducation;
    RemoveEmail;
    RemoveMedia;
    RemoveOther;
    RemoveShopping;
    RemoveSocialMedia;
    RemoveSoftware;
    RemoveWork;
  end
  else if CategorySelection = 'Work' then
  begin
    ShowAll;
    RemoveBanking;
    RemoveEducation;
    RemoveEmail;
    RemoveMedia;
    RemoveOther;
    RemoveShopping;
    RemoveSocialMedia;
    RemoveSoftware;
    RemoveWeb;
  end;
  ListView1.selected :=
    ListView1.FindCaption(-1, CategorySelection, False, False, False, True);
  if ListView1.Selected <> nil then
    // Avoids SegFault when Category is empty
  begin
    ListView1.SetFocus;
    ListView1.Selected.MakeVisible(True);
    ResultName.Caption := ListView1.selected.SubItems[0];
    ResultURL.Text := ListView1.selected.SubItems[1];
    ResultUser.Text := ListView1.selected.SubItems[2];
    ResultPassword.Text := ListView1.selected.SubItems[3];
    image := StrToInt(ListView1.selected.SubItems[4]);
    sitesImageList.GetBitmap(image, ResultImage.Picture.Bitmap);
  end;
  TBEditEntry.Enabled := True; // Allows selected entry to be edited
  TBRemove.Enabled := True; // or deleted
  mnuEdit.Enabled := True;  // Dropdown menu options
  mnuDelete.Enabled := True;
end;

//  BEGIN Remove / Hide categories
procedure TmainLazLock.RemoveBanking;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  // Count number of entries
  ListView1.Items.BeginUpdate;
  try
    for iCF := 0 to ListView1.Items.Count - 1 do
      // loop through entries and remove for Banking
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Banking', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveEducation;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Education', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveEmail;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Email', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveMedia;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Media', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveOther;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Other', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveShopping;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Shopping', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveSocialMedia;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Social Media', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveSoftware;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Software', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveWeb;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Web', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.RemoveWork;
begin
  iCF := 0;   // actual number of entries
  xCF := 0;   // used for the counter
  ListView1.Items.BeginUpdate;
  try
    // Count number of entries
    for iCF := 0 to ListView1.Items.Count - 1 do
      for xCF := 1 to iCF do
      begin
        FilterList := ListView1.FindCaption(0, 'Work', False, True, False);
        if Assigned(FilterList) then
          ListView1.Items.Delete(FilterList.Index);
      end;
  finally
    ListView1.Items.EndUpdate;
  end;
  xCF := 0;
end;

procedure TmainLazLock.ShowAll;
var
  BackupLine: string;
  BackupIndex, BackupComPos: integer;
  item: TListItem;
begin
  ListView1.Clear;
  (*  Fill ListView with entries  *)
  for BackupIndex := 0 to BackupListView.Count - 1 do
  begin
    BackupLine := BackupListView[BackupIndex];
    item := ListView1.Items.Add;
    BackupComPos := Pos(#9, BackupLine);
    item.Caption := Copy(BackupLine, 1, BackupComPos - 1);
    Delete(BackupLine, 1, BackupComPos);
    BackupComPos := Pos(#9, BackupLine);
    while BackupComPos > 0 do
    begin
      item.SubItems.Add(Copy(BackupLine, 1, BackupComPos - 1));
      Delete(BackupLine, 1, BackupComPos);
      BackupComPos := Pos(#9, BackupLine);
    end;
    item.SubItems.Add(BackupLine);
  end;
  IsStringListSaved := True;
end;

//  END Remove / Hide categories

(*  Display login details when the listview is clicked.
   OnMouseDown used instead of OnClick so the Right Click
   context menu will work  *)
procedure TmainLazLock.ListView1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ListViewClicked;
end;

procedure TmainLazLock.SavePrompt; // If file has been amended but not saved
begin
  if MessageDlg(MSGsaveChanges, MSGfileHasBeenChanged, mtConfirmation,
    [mbYes, mbNo], 0) = mrYes then
    EncryptAndSave;
end;

procedure TmainLazLock.TBSaveClick(Sender: TObject); // Click Save
begin
  ShowAll;
  EncryptAndSave;
end;

procedure TmainLazLock.TBSaveMouseEnter(Sender: TObject);
begin
  StatusBar1.SimpleText := STATUSsaveEncrypt;
end;

procedure TmainLazLock.TBSaveMouseLeave(Sender: TObject);
begin
  ClearStatusMessage;
end;

procedure TmainLazLock.copyURLMouseEnter(Sender: TObject);
begin
  CopyMessage;
end;

procedure TmainLazLock.copyURLClick(Sender: TObject);
begin
  ResultURL.SelectAll;
  ResultURL.CopyToClipboard;
end;

procedure TmainLazLock.copyPasswordClick(Sender: TObject);
var
  ClearPassword: string;
  // ResultPassword displays password character '*' so the text is copied to a variable
begin           // instead of copied to the clipboard directly like the URL and Username
  ClearPassword := ResultPassword.Text;
  Clipboard.AsText := ClearPassword;
end;

//  User chooses a category
procedure TmainLazLock.btnShowAllClick(Sender: TObject);
begin
  CategorySelection := 'All';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnSocialMediaClick(Sender: TObject);
begin
  CategorySelection := 'Social Media';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnSoftwareClick(Sender: TObject);
begin
  CategorySelection := 'Software';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnWebClick(Sender: TObject);
begin
  CategorySelection := 'Web';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnWorkClick(Sender: TObject);
begin
  CategorySelection := 'Work';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnBankingClick(Sender: TObject);
begin
  CategorySelection := 'Banking';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnEducationClick(Sender: TObject);
begin
  CategorySelection := 'Education';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnEmailClick(Sender: TObject);
begin
  CategorySelection := 'Email';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnMediaClick(Sender: TObject);
begin
  CategorySelection := 'Media';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnShoppingClick(Sender: TObject);
begin
  CategorySelection := 'Shopping';
  CategoryViewClicked;
end;

procedure TmainLazLock.btnOtherClick(Sender: TObject);
begin
  CategorySelection := 'Other';
  CategoryViewClicked;
end;

procedure TmainLazLock.copyUserClick(Sender: TObject);
begin
  ResultUser.SelectAll;
  ResultUser.CopyToClipboard;
end;

procedure TmainLazLock.FormActivate(Sender: TObject);
// When form first opens, offer to open online help
begin
  if FullShutdown = True then
    Application.Terminate
  // This stops HELP dialog appearing when the login is cancelled but the main form loads
  else
  begin
    if IsNew = True then
    begin
      mainLazLock.AlphaBlendValue := 150;
      if MessageDlg(LetsBegin, WelcomeMessageDialog, mtConfirmation,
        [mbYes, mbNo], 0) = mrYes then
      begin
        OpenURL('https://www.cpunk-security.com/lazlock2.html');
        mainLazLock.AlphaBlendValue := 255;
      end;
      mainLazLock.AlphaBlendValue := 255;
    end;
    IsNew := False;
    WriteToStringList;
    IsStringListSaved := False;
  end;
  LVItemsCount := ListView1.Items.Count;
  ClearStatusMessage;
end;

procedure TmainLazLock.FormClose(Sender: TObject; var CloseAction: TCloseAction);
// Checks if file saved before quitting
begin
  TBQuitClick(Sender);
end;

end.
