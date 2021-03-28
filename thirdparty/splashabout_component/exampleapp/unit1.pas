{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************

= Example application for the SplashAbout component
= Gordon Bamber
= minesadorada@gmail.com
= June 2014
=
= Use this app to experiment with different Splash screens and About dialogs
=
= Note: All external files MUST be deployed with your application in the same folder
}
unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, usplashabout, Forms, Controls, FileUtil, Graphics, Dialogs,
  Buttons, ExtCtrls, StdCtrls, ExtDlgs, Menus, Lresources,uStringListEditor;

type

  { TForm1 }

  TForm1 = class(TForm)
    cmd_Help: TBitBtn;
    cmd_EditLicenseTxt: TButton;
    cmd_Description: TButton;
    cmd_EditCreditsTxt: TButton;
    cmd_SetFont: TButton;
    cmd_CreditsLines: TButton;
    cmd_SetAuthor: TButton;
    cmd_SetOrganisation: TButton;
    cmd_SetSupportContact: TButton;
    cmd_SetUserTitle: TButton;
    CheckGroupUseExternalFile: TCheckGroup;
    cmd_BitmapBackground: TButton;
    cmd_Icon: TButton;
    cmd_BitmapMaskMono: TButton;
    cmd_BitmapMaskColor: TButton;
    cmd_TestAbout: TBitBtn;
    cmd_TestSplash: TBitBtn;
    cmd_Close: TBitBtn;
    CheckGroupShow: TCheckGroup;
    edt_SplashHeight: TLabeledEdit;
    edt_AboutHeight: TLabeledEdit;
    edt_AboutWidth: TLabeledEdit;
    CreditsGroupBox: TGroupBox;
    FontDialog1: TFontDialog;
    FontGroupBox: TGroupBox;
    MainMenu1: TMainMenu;
    mnu_helpAbout: TMenuItem;
    mnu_fileHelp: TMenuItem;
    mnu_fileClose: TMenuItem;
    mnu_file: TMenuItem;
    ResizeGraphicRadioGroup: TRadioGroup;
    SetLicenseVarsGroupBox: TGroupBox;
    GroupBoxBitmaps: TGroupBox;
    GroupBoxTitleStyle: TGroupBox;
    GroupBoxSplashDialog: TGroupBox;
    edt_SplashWidth: TLabeledEdit;
    GroupBoxAboutDialog: TGroupBox;
    MaskTypeRadioGroup: TRadioGroup;
    GraphicsRadioGroup: TRadioGroup;
    dlg_OpenBitmap: TOpenPictureDialog;
    LicenseFileRadioGroup: TRadioGroup;
    txt_Top: TStaticText;
    TitleStyleRadioGroup: TRadioGroup;
    SplashAlignRadioGroup: TRadioGroup;
    SplashAbout1: TSplashAbout;
    AboutAlignRadioGroup: TRadioGroup;
    procedure AboutAlignRadioGroupSelectionChanged(Sender: TObject);
    procedure CheckGroupUseExternalFileClick(Sender: TObject);
    procedure CheckGroupUseExternalFileItemClick(Sender: TObject; Index: integer);
    procedure cmd_BitmapBackgroundClick(Sender: TObject);
    procedure cmd_CreditsLinesClick(Sender: TObject);
    procedure cmd_DescriptionClick(Sender: TObject);
    procedure cmd_EditCreditsTxtClick(Sender: TObject);
    procedure cmd_EditLicenseTxtClick(Sender: TObject);
    procedure cmd_HelpClick(Sender: TObject);
    procedure cmd_IconClick(Sender: TObject);
    procedure cmd_BitmapMaskColorClick(Sender: TObject);
    procedure cmd_BitmapMaskMonoClick(Sender: TObject);
    procedure cmd_SetAuthorClick(Sender: TObject);
    procedure cmd_SetFontClick(Sender: TObject);
    procedure cmd_SetOrganisationClick(Sender: TObject);
    procedure cmd_SetSupportContactClick(Sender: TObject);
    procedure cmd_SetUserTitleClick(Sender: TObject);
    procedure cmd_TestAboutClick(Sender: TObject);
    procedure cmd_TestSplashClick(Sender: TObject);
    procedure CheckGroupShowItemClick(Sender: TObject; Index: integer);
    procedure edt_AboutHeightEditingDone(Sender: TObject);
    procedure edt_AboutWidthEditingDone(Sender: TObject);
    procedure edt_SplashHeightEditingDone(Sender: TObject);
    procedure edt_SplashWidthEditingDone(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GraphicsRadioGroupSelectionChanged(Sender: TObject);
    procedure LicenseFileRadioGroupSelectionChanged(Sender: TObject);
    procedure MaskTypeRadioGroupSelectionChanged(Sender: TObject);
    procedure mnu_helpAboutClick(Sender: TObject);
    procedure ResizeGraphicRadioGroupSelectionChanged(Sender: TObject);
    procedure SplashAlignRadioGroupSelectionChanged(Sender: TObject);
    procedure TitleStyleRadioGroupSelectionChanged(Sender: TObject);
  private
    { private declarations }
  var
    i: integer; //(Used in TryStrToInt routines)
    sz: string; // used in InputQuery routines
    ABitMap: TBitmap;

    function FetchBitmap(const AFileFilter: string): boolean;
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.cmd_TestAboutClick(Sender: TObject);
begin
  SplashAbout1.ShowAbout;
end;

procedure TForm1.AboutAlignRadioGroupSelectionChanged(Sender: TObject);
begin
  SplashAbout1.FormAboutTextAlign := TAlignment(AboutAlignRadioGroup.ItemIndex);
end;

procedure TForm1.CheckGroupUseExternalFileClick(Sender: TObject);
begin

end;

procedure TForm1.CheckGroupUseExternalFileItemClick(Sender: TObject; Index: integer);
const
  C_NOBITMAP = 'There is no graphic currently assigned. Click the button to assign one';
  C_CLEAR_RESOURCE = 'Clear the graphic resource?';
  // There's probably a more elegant way to do this...
begin
  if (SplashAbout1.Graphics = saExternalFiles) then
    // Set the ExternalFileOptions flags
  begin
    case Index of
      0: if CheckGroupUseExternalFile.Checked[Index] then
          SplashAbout1.ExternalFileOptions :=
            SplashAbout1.ExternalFileOptions + [saExternalBackground]
        else
          SplashAbout1.ExternalFileOptions :=
            SplashAbout1.ExternalFileOptions - [saExternalBackground];
      1: if CheckGroupUseExternalFile.Checked[Index] then
          SplashAbout1.ExternalFileOptions :=
            SplashAbout1.ExternalFileOptions + [saExternalMaskMonoImage]
        else
          SplashAbout1.ExternalFileOptions :=
            SplashAbout1.ExternalFileOptions - [saExternalMaskMonoImage];
      2: if CheckGroupUseExternalFile.Checked[Index] then
          SplashAbout1.ExternalFileOptions :=
            SplashAbout1.ExternalFileOptions + [saExternalMaskColorImage]
        else
          SplashAbout1.ExternalFileOptions :=
            SplashAbout1.ExternalFileOptions - [saExternalMaskColorImage];
      3: if CheckGroupUseExternalFile.Checked[Index] then
          SplashAbout1.ExternalFileOptions :=
            SplashAbout1.ExternalFileOptions + [saExternalIcon]
        else
          SplashAbout1.ExternalFileOptions :=
            SplashAbout1.ExternalFileOptions - [saExternalIcon];
    end;
  end
  else
  begin
    // Graphics=saResources so a different set of actions...
    case Index of
      0: if CheckGroupUseExternalFile.Checked[Index] then
        begin
          if (SplashAbout1.BitmapBackGround.Width = 0) then
          begin
            ShowMessage(C_NOBITMAP);
            CheckGroupUseExternalFile.Checked[Index] := False;
          end;
        end
        else
        begin
          if (SplashAbout1.BitmapBackGround.Width > 0) then
            if MessageDlg(C_CLEAR_RESOURCE, mtConfirmation,
              [mbYes, mbAbort], 0) = mrYes then
              SplashAbout1.BitmapBackGround := nil
            else
              CheckGroupUseExternalFile.Checked[Index] := True;
        end;
      1: if CheckGroupUseExternalFile.Checked[Index] then
        begin
          if (SplashAbout1.BitmapMaskMono.Width = 0) then
          begin
            ShowMessage(C_NOBITMAP);
            CheckGroupUseExternalFile.Checked[Index] := False;
          end;
        end
        else
        begin
          if (SplashAbout1.BitmapMaskMono.Width > 0) then
            if MessageDlg(C_CLEAR_RESOURCE, mtConfirmation,
              [mbYes, mbAbort], 0) = mrYes then
              SplashAbout1.BitmapMaskMono := nil
            else
              CheckGroupUseExternalFile.Checked[Index] := True;
        end;
      2: if CheckGroupUseExternalFile.Checked[Index] then
        begin
          if (SplashAbout1.BitmapMaskColor.Width = 0) then
          begin
            ShowMessage(C_NOBITMAP);
            CheckGroupUseExternalFile.Checked[Index] := False;
          end;
        end
        else
        begin
          if (SplashAbout1.BitmapMaskColor.Width > 0) then
            if MessageDlg(C_CLEAR_RESOURCE, mtConfirmation,
              [mbYes, mbAbort], 0) = mrYes then
              SplashAbout1.BitmapMaskColor := nil
            else
              CheckGroupUseExternalFile.Checked[Index] := True;
        end;
      3: if CheckGroupUseExternalFile.Checked[Index]=False then
            CheckGroupUseExternalFile.Checked[Index] := True;
    end;
  end;
end;

function TForm1.FetchBitmap(const AFileFilter: string): boolean;
  // Open a graphics dialog with specified filter
  // Use LoadFromFile to assign to a graphic type
  // convert various graphic types into TBitmap and
  // assign it to the private var ABitmap

  // Used to assign to TBitmap properties of SplashAbout
var
  MyBitMap: TBitMap;
  MyJPG: TJpegImage;
  MyTiff: TTiffImage;
  MyGif: TGifImage;
  MyPNG: TPortableNetworkGraphic;
  szTemp: string;
begin
  try
    MyBitmap := TBitMap.Create;
    MyJPG := TJpegImage.Create;
    MyTiff := TTiffImage.Create;
    // Turn off pointless compiler warning about creating a TGifImage
    {$WARNINGS OFF}
    MyGif := TGifImage.Create;
    {$WARNINGS ON}
    MyPNG := TPortableNetworkGraphic.Create;
    szTemp := dlg_OpenBitmap.Filter;
    Result := False;
    dlg_OpenBitmap.Title := 'Fetch graphic';
    dlg_OpenBitmap.Filter := 'Graphic|' + AFileFilter;
    if dlg_OpenBitmap.Execute then
    begin
      if CompareFileExt(dlg_OpenBitmap.Filename, '.jpg', False) = 0 then
      begin
        MyJPG.LoadFromFile(dlg_OpenBitmap.Filename);
        ABitMap.Assign(MyJPG);
      end;
      if CompareFileExt(dlg_OpenBitmap.Filename, '.bmp', False) = 0 then
      begin
        MyBitMap.LoadFromFile(dlg_OpenBitmap.Filename);
        ABitMap.Assign(MyBitMap);
      end;
      if CompareFileExt(dlg_OpenBitmap.Filename, '.tif', False) = 0 then
      begin
        MyTiff.LoadFromFile(dlg_OpenBitmap.Filename);
        ABitMap.Assign(MyTiff);
      end;
      if CompareFileExt(dlg_OpenBitmap.Filename, '.png', False) = 0 then
      begin
        MyPNG.LoadFromFile(dlg_OpenBitmap.Filename);
        ABitMap.Assign(MyPNG);
      end;
      if CompareFileExt(dlg_OpenBitmap.Filename, '.gif', False) = 0 then
      begin
        MyGif.LoadFromFile(dlg_OpenBitmap.Filename);
        ABitMap.Assign(MyGif);
      end;
      Result := True;
    end;
  finally
    dlg_OpenBitmap.Filter := szTemp;
    MyPNG.Free;
    MyGif.Free;
    MyTiff.Free;
    MyJPG.Free;
    MyBitmap.Free;
  end;
end;

procedure TForm1.cmd_BitmapBackgroundClick(Sender: TObject);
begin
  if (SplashAbout1.Graphics = saResources) then
  begin
    if FetchBitmap('*.bmp;*.jpg;*.tif;*.png;*.gif;') then
      SplashAbout1.BitmapBackGround.Assign(ABitMap);
  end
  else
  begin
    dlg_OpenBitmap.Title := 'Fetch external graphic file';
    dlg_OpenBitmap.Filename := SplashAbout1.ExternalFileBackgroundImage;
    if dlg_OpenBitmap.Execute then
    begin
      SplashAbout1.ExternalFileBackgroundImage := dlg_OpenBitmap.Filename;
      if (GraphicsRadioGroup.ItemIndex <> 0) then;
    end;
  end;
end;

procedure TForm1.cmd_CreditsLinesClick(Sender: TObject);
begin
  with frm_StringListEditor do
  begin
    Caption := 'Edit CreditLines';
    StringListEditorMemo.Clear;
    StringListEditorMemo.Lines.Assign(SplashAbout1.Creditlines);
    ShowModal;
    SplashAbout1.Creditlines.Assign(StringListEditorMemo.Lines);
  end;
end;

procedure TForm1.cmd_DescriptionClick(Sender: TObject);
begin
  with frm_StringListEditor do
  begin
    Caption := 'Edit Description';
    StringListEditorMemo.Clear;
    StringListEditorMemo.Lines.Assign(SplashAbout1.Description);
    ShowModal;
    SplashAbout1.Description.Assign(StringListEditorMemo.Lines);
  end;
end;

procedure TForm1.cmd_EditCreditsTxtClick(Sender: TObject);
begin
  // Warn user
  if (not FileExists(SplashAbout1.CreditsTextFilename)) then
    ShowMessageFmt(
      'Note that if you create and populate ''%s'' it will be used in preference to the CreditLines stringlist',
      [SplashAbout1.CreditsTextFilename]);

  with frm_StringListEditor do
  begin
    StringListEditorMemo.Clear;
    // Load from file?
    if FileExists(SplashAbout1.CreditsTextFilename) then
      StringListEditorMemo.Lines.LoadFromFile(SplashAbout1.CreditsTextFilename);
    // Show the editor
    ShowModal;
    // Only (create) and Save if there is text in the Memo
    // Note this overwrites any existing file
    if StringListEditorMemo.Lines.Count > 0 then
      StringListEditorMemo.Lines.SaveToFile(SplashAbout1.CreditsTextFilename)
    else
    // Clearing the text and saving deletes any existing file
    // No point in saving an empty file
    if FileExists(SplashAbout1.CreditsTextFilename) then
      SysUtils.DeleteFile(SplashAbout1.CreditsTextFilename);
  end;
end;

procedure TForm1.cmd_EditLicenseTxtClick(Sender: TObject);
begin
  with frm_StringListEditor do
  begin
    StringListEditorMemo.Clear;
    if FileExists('license.txt') then
      StringListEditorMemo.Lines.LoadFromFile('license.txt');
    ShowModal;
    if StringListEditorMemo.Lines.Count > 0 then
      StringListEditorMemo.Lines.SaveToFile('license.txt')
    else
    begin
      if FileExists('license.txt') then
      begin
        FileUtil.CopyFile('license.txt', 'license.txt.bak');
        DeleteFile('license.txt');
        ShowMessage('Old ''license.txt'' has been backed up to ''license.txt.bak''');
      end;
    end;
  end;
end;

procedure TForm1.cmd_HelpClick(Sender: TObject);
var
  iPos: integer;
begin
  with frm_StringListEditor do
  begin
    Caption := 'Help for SplashAbout tester';
    StringListEditorMemo.Clear;
    StringListEditorMemo.Lines.LoadFromFile('readme.txt');
    // Highlight the relavent text
    iPos := Pos('==If', StringListEditorMemo.Text);
    StringListEditorMemo.SelStart := iPos - 1;
    StringListEditorMemo.SelLength := 98;
    // Show the readonly memo
    StringListEditorMemo.ReadOnly := True;
    ShowModal;
    StringListEditorMemo.ReadOnly := False;
  end;
end;

procedure TForm1.cmd_IconClick(Sender: TObject);
var
  szTemp: string;
begin
  Try
  if (SplashAbout1.Graphics = saResources) then
  begin
    szTemp := dlg_OpenBitmap.Filter;
    dlg_OpenBitmap.Filter := 'Icon|*.ico';
    if dlg_OpenBitmap.Execute then
    begin
      SplashAbout1.Icon.LoadFromFile(dlg_OpenBitmap.Filename);
    end;
    dlg_OpenBitmap.Filter := szTemp;
  end
  else
  begin
    dlg_OpenBitmap.Filename := SplashAbout1.ExternalFileIcon;
    if dlg_OpenBitmap.Execute then
    begin
      SplashAbout1.ExternalFileIcon := dlg_OpenBitmap.Filename;
      if (GraphicsRadioGroup.ItemIndex <> 0) then
      begin
        GraphicsRadioGroup.ItemIndex := 0;
        ShowMessage('Graphics property has been set to saExternalFiles');
      end;
    end;
  end;
  Except
    on e: Exception do
    ShowMessage(e.ClassName);
  end;
end;

procedure TForm1.cmd_BitmapMaskColorClick(Sender: TObject);
begin
  if (SplashAbout1.Graphics = saResources) then
  begin
    if FetchBitmap('*.bmp;*.jpg;*.tif;*.png;*.gif;') then
      SplashAbout1.BitmapMaskColor.Assign(ABitMap);
  end
  else
  begin
    dlg_OpenBitmap.Filename := SplashAbout1.ExternalFileMaskColorImage;
    if dlg_OpenBitmap.Execute then
    begin
      SplashAbout1.ExternalFileMaskColorImage := dlg_OpenBitmap.Filename;
      if (GraphicsRadioGroup.ItemIndex <> 0) then
      begin
        GraphicsRadioGroup.ItemIndex := 0;
        ShowMessage('Graphics property has been set to saExternalFiles');
      end;
    end;
  end;
end;

procedure TForm1.cmd_BitmapMaskMonoClick(Sender: TObject);
begin
  if (SplashAbout1.Graphics = saResources) then
  begin
    if FetchBitmap('*.bmp;') then
      SplashAbout1.BitmapMaskMono.Assign(ABitMap);
  end
  else
  begin
    dlg_OpenBitmap.Filename := SplashAbout1.ExternalFileMaskMonoImage;
    if dlg_OpenBitmap.Execute then
    begin
      SplashAbout1.ExternalFileMaskMonoImage := dlg_OpenBitmap.Filename;
      if (GraphicsRadioGroup.ItemIndex <> 0) then
      begin
        GraphicsRadioGroup.ItemIndex := 0;
        ShowMessage('Graphics property has been set to saExternalFiles');
      end;
    end;
  end;
end;

procedure TForm1.cmd_SetAuthorClick(Sender: TObject);
begin
  if InputQuery('Set the Author token', 'Current value = ' +
    SplashAbout1.Author, sz) then
    SplashAbout1.Author := sz;
end;

procedure TForm1.cmd_SetFontClick(Sender: TObject);
begin
{$IFDEF WINDOWS}
  // Warning: 'THandle is depreciated'
{$WARNINGS OFF}
  FontDialog1.Font.Name := GetFontData(cmd_SetFont.Font.Handle).Name;
{$WARNINGS ON}
{$ENDIF}
  if FontDialog1.Execute then
    SplashAbout1.Font.Assign(FontDialog1.Font);
end;

procedure TForm1.cmd_SetOrganisationClick(Sender: TObject);
begin
  if InputQuery('Set the Organisation token', 'Current value = ' +
    SplashAbout1.Organisation, sz) then
    SplashAbout1.Organisation := sz;
end;

procedure TForm1.cmd_SetSupportContactClick(Sender: TObject);
begin
  if InputQuery('Set the SupportContact token', 'Current value = ' +
    SplashAbout1.SupportContact, sz) then
    SplashAbout1.SupportContact := sz;
end;


procedure TForm1.cmd_SetUserTitleClick(Sender: TObject);
begin
  if InputQuery('Set the User Title', 'Current value = ' +
    SplashAbout1.UserTitle, sz) then
    SplashAbout1.UserTitle := sz;
end;

procedure TForm1.cmd_TestSplashClick(Sender: TObject);
begin
  SplashAbout1.ShowSplash;
end;

procedure TForm1.CheckGroupShowItemClick(Sender: TObject; Index: integer);
begin
  with CheckGroupShow do
    case Index of
      0: SplashAbout1.ShowPoweredBy := Checked[0];
      1: SplashAbout1.ShowDescription := Checked[1];
      2: SplashAbout1.ShowCreditButton := Checked[2];
    end;
end;

procedure TForm1.edt_AboutHeightEditingDone(Sender: TObject);
begin
  if TryStrToInt(edt_AboutHeight.Text, i) then
    SplashAbout1.FormAboutHeight := i
  else
  begin
    ShowMessageFmt('%s is not a number', [edt_AboutHeight.Text]);
    edt_AboutHeight.Text := IntToStr(SplashAbout1.FormAboutHeight);
  end;

end;

procedure TForm1.edt_AboutWidthEditingDone(Sender: TObject);
begin
  if TryStrToInt(edt_AboutWidth.Text, i) then
    SplashAbout1.FormAboutWidth := i
  else
  begin
    ShowMessageFmt('%s is not a number', [edt_AboutWidth.Text]);
    edt_AboutWidth.Text := IntToStr(SplashAbout1.FormAboutWidth);
  end;

end;

procedure TForm1.edt_SplashHeightEditingDone(Sender: TObject);
begin
  if TryStrToInt(edt_SplashHeight.Text, i) then
    SplashAbout1.FormSplashHeight := i
  else
  begin
    ShowMessageFmt('%s is not a number', [edt_SplashHeight.Text]);
    edt_SplashHeight.Text := IntToStr(SplashAbout1.FormSplashHeight);
  end;

end;

procedure TForm1.edt_SplashWidthEditingDone(Sender: TObject);
begin
  if TryStrToInt(edt_SplashWidth.Text, i) then
    SplashAbout1.FormSplashWidth := i
  else
  begin
    ShowMessageFmt('%s is not a number', [edt_SplashWidth.Text]);
    edt_SplashWidth.Text := IntToStr(SplashAbout1.FormSplashWidth);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SplashAbout1.ShowSplash;
  // Init the Example app controls

  // RadioGroups (enumerated properties)
  MaskTypeRadioGroup.ItemIndex := Ord(SplashAbout1.MaskType);
  TitleStyleRadioGroup.ItemIndex := Ord(SplashAbout1.TitleStyle);
  SplashAlignRadioGroup.ItemIndex := Ord(SplashAbout1.FormSplashTextAlign);
  AboutAlignRadioGroup.ItemIndex := Ord(SplashAbout1.FormAboutTextAlign);
  LicenseFileRadioGroup.ItemIndex := Ord(SplashAbout1.LicenseFile);
  ResizeGraphicRadioGroup.ItemIndex := Ord(SplashAbout1.ResizeMode);
  // Enumerated type with 2 items acts like booleans (!)
  if SplashAbout1.Graphics = saExternalFiles then
    GraphicsRadioGroup.ItemIndex := 0
  else
    GraphicsRadioGroup.ItemIndex := 1;
  with CheckGroupShow do

    // Checkboxes
  begin
    if SplashAbout1.ShowPoweredBy then
      Checked[0] := True;
    if SplashAbout1.ShowDescription then
      Checked[1] := True;
    if SplashAbout1.ShowCreditButton then
      Checked[2] := True;
  end;
  with CheckGroupUseExternalFile do
  begin
    if saExternalBackground in SplashAbout1.ExternalFileOptions then
      Checked[0] := True;
    if saExternalMaskMonoImage in SplashAbout1.ExternalFileOptions then
      Checked[1] := True;
    if saExternalMaskColorImage in SplashAbout1.ExternalFileOptions then
      Checked[2] := True;
    if saExternalIcon in SplashAbout1.ExternalFileOptions then
      Checked[3] := True;
  end;

  // Edit controls
  edt_SplashWidth.Text := IntToStr(SplashAbout1.FormSplashWidth);
  edt_SplashHeight.Text := IntToStr(SplashAbout1.FormSplashHeight);
  edt_AboutWidth.Text := IntToStr(SplashAbout1.FormAboutWidth);
  edt_AboutHeight.Text := IntToStr(SplashAbout1.FormAboutHeight);

  // Buttons
  cmd_EditCreditsTxt.Caption := 'Edit ' + SplashAbout1.CreditsTextfileName;

  ABitmap := TBitmap.Create;
end;

procedure TForm1.GraphicsRadioGroupSelectionChanged(Sender: TObject);
Var iCount:Integer;
begin
  case GraphicsRadioGroup.ItemIndex of
    0: SplashAbout1.Graphics := saExternalFiles;
    1: SplashAbout1.Graphics := saResources;
  end;
  // Reset the 'Use' checkboxes
  For iCount:=0 to 3 do
      CheckGroupUseExternalFile.Checked[iCount]:=True;
end;

procedure TForm1.LicenseFileRadioGroupSelectionChanged(Sender: TObject);
begin
  SplashAbout1.LicenseFile := tLicenseFile(LicenseFileRadioGroup.ItemIndex);
end;

procedure TForm1.MaskTypeRadioGroupSelectionChanged(Sender: TObject);
begin
  SplashAbout1.MaskType := tMaskType(MaskTypeRadioGroup.ItemIndex);
end;

procedure TForm1.mnu_helpAboutClick(Sender: TObject);
// You can set Splashabout properties via code:
Var
  tempTitleStyle:TTitleStyleType;
  tempTitle:String;
begin
  With SplashAbout1 do
  begin
       tempTitleStyle:=TitleStyle;
       tempTitle:=UserTitle;
       TitleStyle:=saUserTitle;
       UserTitle:=txt_Top.Caption;
       ShowAbout;
       TitleStyle:=tempTitleStyle;
       UserTitle:=tempTitle;
  end;
end;

procedure TForm1.ResizeGraphicRadioGroupSelectionChanged(Sender: TObject);
begin
    SplashAbout1.ResizeMode:=tResizeMode(ResizeGraphicRadioGroup.ItemIndex);

end;

procedure TForm1.SplashAlignRadioGroupSelectionChanged(Sender: TObject);
begin
  SplashAbout1.FormSplashTextAlign := TAlignment(SplashAlignRadioGroup.ItemIndex);
end;

procedure TForm1.TitleStyleRadioGroupSelectionChanged(Sender: TObject);
begin
  SplashAbout1.TitleStyle := TTitleStyleType(TitleStyleRadioGroup.ItemIndex);
end;
// When compiling, explain the 'platform' directive on some properties
{$Hint Don't worry about the 'Warning: Symbol xxx is not portable' messages}
{$Hint The 'not portable' code is stuff that won't work outside of Windows}
{$Hint ..so I flagged it as such in the component code}

end.
