unit usplashabout;

{$mode objfpc}{$H+}
interface

{
Credits
=======
uversion.pas by Mike Thompson - mike.cornflake@gmail.com
originally as VersionSupport.pas
See uversion.pas header for more details
All other code by Gordon Bamber - minesadorada@charcodelvalle.com

========================================================
SplashAbout component by minesadorada@charcodelvalle.com
========================================================
Licence: LGPL licence.  Free to use and/or abuse in your code.

Purpose
=======
Constructs a Splash screen and/or About form with as little effort and resources as possible :)
The windows and controls are created and destroyed on-the-fly to reduce application size
Choice to only use externally-deployed graphics to reduce executable size
For use in any Lazarus 1.x/FPC 2.x application

Files needed to install component
=================================================
uspalshabout.lpk usplashabout.pas,uversion.pas,
license.lrs,masks.lrs,splashabout_ico.lrs (resource files)

Optional Files in the same folder as your application (ONLY if Graphics=saExternalFiles)
========================================================================================
Optional: <mysplashgraphic>.(bmp/jpg/gif)
Optional: <mysplashicon>.ico
Optional: <mymaskmonographic>.bmp
Optional: <mymaskcolorgraphic>.jpg

How to use in your project (see example project)
================================================
Use the Property Editor to set propertes and call ShowSplash and/or ShowAbout in your form code

== Property tips ==
// If SplashAbout1.DelaySeconds set to zero, then SplashScreen will close only when your application becomes idle
// TitleStyle allows you to use default Application or Form icons and titles or set your own (via Icon and UserTitle properties)
//
// == External files or built-in resources? (Graphics=saResources or Graphics=saExternalFiles) ==
// Using saResources will increase the size of your executable whereas saExternalFiles will not
// HOWEVER any external files specified MUST be deployed in the same folder as your executable.
// If Graphics=saResources, then (optionally) set the properties:
//    BitmapBackGround, BitmapMaskColor, BitmapMaskMono and Icon
// If Graphics=saExternalFiles then (optionally) set the properties
//    ExternalFileBackGroundImage,ExternalFileIcon,ExternalFileMaskColorImage and ExternalFileMonoImage
//    * Note that ExternalFileOptions flags can control which are displayed
// The optional properties MaskMonoImage and MaskColour Image
// can make a shaped splash form (See the example app for ideas)
// For MaskMonoImage, optimum source: .BMP image, 2-bit color depth or more, black area=invisible, white area=visible. Size >= 320 x 240
// If a jpg file is specified by mistake, it will be converted to a (large filesize 24bbp!) .bmp and saved to disk on first run.
// For MaskColorImage, use a standard colour JPG file with a white background
// This allows a shaped jpg image to be shown as a splash screen - very dramatic!
//
// SplashAbout1.LicenseFile ; // Default is for Licence button to be absent on ShowAbout method
// SplashAbout1.CreditText:='Freeware by minesadorada'; // OPTIONAL: Default is no text
// SplashAbout1.Author:='Mines A. Dorada'; // OPTIONAL.  Default is boilerplate text in LicenseFile Path
// SplashAbout1.SupportContact:='minesadorada@charcodelvalle.com'; // OPTIONAL.  Default is boilerplate text in LicenseFile Path
// SplashAbout1.Description:='My description'; // Will replace build info text if ShowDescription=True

=====================================
EXAMPLE USE in the form create method
=====================================
procedure TForm1.FormCreate(Sender: TObject);
begin
     SplashAbout1.ShowSplash;
end;

=============================
EXAMPLE USE in About.. button
=============================
procedure TForm1.Button1Click(Sender: TObject);
begin
     SplashAbout1.ShowAbout;
end;
}
uses
  Buttons, Classes, Controls, Forms, Graphics, SysUtils,
  LCLIntf, LCLType, LResources,
  Dialogs, ExtCtrls, PropEdits, ScrollTextClass, StdCtrls, StrUtils, uversion,uPoweredBy;

type
  tFormType = (fSplash, fAbout); // Same dialog used for both
  // The license text is stored in the component as a resource.
  // If saUserFile then a file 'license.txt' must be present in the same folder as the executeable
  tLicenseFile = (saNone, saGPL, saLGPL, saMIT, saModifiedGPL, saUserFile);
  // To use your own icon and title then use saUserTitle
  TTitleStyleType = (saUserTitle, saForm, saApplication);
  // List of built-in shaped screens.  Use saUserImage for your own shaped screen.
  tmasktype = (saNomask, saRoundedRect, saBigCog, saBigFlower, saBigSplash, saUserImage);
  // Default is to use internal graphics resources (BitMap.. properties)
  tGraphicType = (saExternalFiles, saResources);
  // Default is saResizeGraphic
  tResizeMode = (saNoResize, saResizeGraphic, saResizeWindow);
  // Used to make up an options list (when Graphics=saExternalFiles)
  tExternalFileOptionsType = (saExternalBackground, saExternalIcon,
    saExternalMaskColorImage, saExternalMaskMonoImage);
  tExternalFileOptions = set of tExternalFileOptionsType;
  // Used in property editor (TypeInfo cast)
  tSplashAboutString = string;

const
  C_DEFAULTSPLASHWIDTH = 320;
  C_DEFAULTSPLASHHEIGHT = 240;
  C_LINUXEXTRASPLASHHEIGHT = 40;
  C_DEFAULTSPLASHHEIGHT_LINUX = C_DEFAULTSPLASHHEIGHT + C_LINUXEXTRASPLASHHEIGHT;

  C_DEFAULTLICENSEFORMWIDTH = 500;
  C_DEFAULTLICENSEFORMWIDTH_LINUX = C_DEFAULTLICENSEFORMWIDTH + 100;
  C_DEFAULTLICENSEFORMHEIGHT = 400;
  C_DEFAULTLICENSEFORMHEIGHT_LINUX = C_DEFAULTLICENSEFORMHEIGHT + 50;
  C_SPLASHABOUT_ERROR = 'SplashAbout Component Error';
  C_SPLASHABOUT_VERSION = '1.4.3.0';

type
  TSplashAbout = class(TComponent)
  private
    fSplashForm: TForm;
    fFormType: tFormType;
    fIcon: TIcon;
    fDelaySeconds: integer;
    fTitleString: string;
    fBackGroundColor: TColor;
    fExternalFileIcon, fExternalFileBackGroundImage, fExternalFileMaskMonoImage,
    fExternalFileMaskColorImage: string;
    fBackGroundBitmap, fMaskMonoBitmap, fMaskColorBitmap: TBitMap;
    fVersionInfoString: string;
    fAppVersionString: string;
    fLicenseFile: tLicenseFile;
    fLicenseTypeString: string;
    fOrganisationString: string;
    fAuthorString: string;
    fSupportContactString: string;
    fCloseOnIdle: boolean;
    szLicenseFile: string;
    fTitleStyle: TTitleStyleType;
    fShowDescription: boolean;
    fDescriptionStringList: TStrings;
    fFormTitleString: string;
    fFormIcon: TIcon;
    fMaskType: tMaskType;
    fAboutString: tSplashAboutString;
    fGraphicType: tGraphicType;
    fExternalFileOptions: tExternalFileOptions;
    fFont: TFont;
    fPoweredBy:TPoweredBy;

    // Credits screen vars
    fCreditLines: TStrings;
    fShowCreditButton: boolean;
    fCreditsTextfileName: string;

    fFormSplashWidth, fFormSplashHeight: integer;
    fFormAboutWidth, fFormAboutHeight: integer;
    fFormSplashTextAlign: TAlignment;
    fFormAboutTextAlign: TAlignment;
    fVersionString: string;
    fResizeMode: tResizeMode;

    fShowPoweredBy: boolean;

    // Used by various buttons to close Splash or About foerm
    procedure CloseForm(Sender: TObject);
    // Main method to construct a splash or about dialog
    procedure ShowForm;
    // Called From ShowForm
    procedure ShowLicense(Sender: TObject);
    // Called when credits button is clicked
    procedure ShowCredits(Sender: TObject);
    // Only called is a developer specifies a jpg 2-colour image by mistake
    function MakeBMPfromJPG(var JPGFilePath: string): boolean;
    // Used when DelaySeconds=0
    procedure ApplicationOnIdle(Sender: TObject; var {%H-}Done: boolean);
    // Property Set procedure
    procedure SetBackGroundBitmap(const AValue: TBitmap);
    // Property Set procedure
    procedure SetMaskMonoBitmap(const AValue: TBitmap); platform;
    // Property Set procedure
    procedure SetMaskColorBitmap(const AValue: TBitmap); platform;
    // Property Set procedure
    procedure SetStrings(const AValue: TStrings);
    // Property Set procedure
    procedure SetCreditLines(const AValue: TStrings);
    // Property Set procedure
    procedure SetFont(const AValue: TFont);
  protected
  public
    // Called when component is dropped onto a form
    constructor Create(AOwner: TComponent); override;
    // Called when component is deleted from a form
    destructor Destroy; override;
    // Usually called in Form.Create
    procedure ShowSplash;
    // Usually called from Help/About menu click
    procedure ShowAbout;
   published
    // Duration of the Splash screen
    property DelaySeconds: integer read fDelaySeconds write fDelaySeconds default 2;
    // Colour of the Splash and About dialogs if no background image specified
    property BackGroundColor: TColor read fBackGroundColor
      write fBackGroundColor default clSkyBlue;
    // If Titlestyle is set to saUserTitle this .ico file can be displayed
    property ExternalFileIcon: string read fExternalFileIcon write fExternalFileIcon;
    // If Graphics is set to saExternalFiles this .jpg will be used
    property ExternalFileBackGroundImage: string
      read fExternalFileBackGroundImage write fExternalFileBackGroundImage;
    // If Graphics is set to saExternalFiles this .bmp will be used
    property ExternalFileMaskMonoImage: string
      read fExternalFileMaskMonoImage write fExternalFileMaskMonoImage; platform;
    // If Graphics is set to saExternalFiles this .jpg will be used
    property ExternalFileMaskColorImage: string
      read fExternalFileMaskColorImage write fExternalFileMaskColorImage; platform;
    // These options allow you to experiment with external file settings
    property ExternalFileOptions: tExternalFileOptions
      read fExternalFileOptions write fExternalFileOptions default
      [saExternalBackground, saExternalIcon, saExternalMaskColorImage,
      saExternalMaskMonoImage];
    // Sets the font for all text on the Splash and About dialogs
    property Font: TFont read fFont write SetFont;
    // If Graphics is set to saResources this bitmap will be used
    property BitmapBackGround: TBitMap read fBackGroundBitmap write SetBackGroundBitmap;
    // If Graphics is set to saResources this bitmap will be used
    property BitmapMaskMono: TBitmap read fMaskMonoBitmap write SetMaskMonoBitmap;
      platform;
    // If Graphics is set to saResources this bitmap will be used
    property BitmapMaskColor: TBitMap read fMaskColorBitmap write SetMaskColorBitmap;
      platform;
    // User icon used if Graphics=saResources and TitleStyle=saUserTitle
    property Icon: TIcon read fIcon write fIcon;
    // Default:saNoMask Set saUserImage to use a shaped splash screen and saNoMask for a standard screen
    property MaskType: tMaskType read fMaskType write fMaskType default saNomask;
    // If not saNone then a [License] button will be available on the About Dialog
    property LicenseFile: tLicenseFile
      read fLicenseFile write fLicenseFile default saNone;
    // Determines whether to use built-in bitmaps or external files
    property Graphics: tGraphicType read fGraphicType write fGraphicType default
      saResources;
    // Your Organisation is displayed in the About Dialog and the License screen
    property Organisation: string read fOrganisationString write fOrganisationString;
    // Displayed in the license screen
    property SupportContact: string read fSupportContactString
      write fSupportContactString;
    // Displayed in the license screen
    property Author: string read fAuthorString write fAuthorString;
    // saApplication uses the Application properties, saForm uses the Form properties
    property TitleStyle: TTitleStyleType
      read fTitleStyle write fTitleStyle default saApplication;
    // If saTitleStyle=saUserTitle then this value is displayed
    property UserTitle: string read fTitleString write fTitleString;
    // If TRUE then the Description property text is displayed in the About dialog
    property ShowDescription: boolean read fShowDescription write fShowDescription;
    // If ShowDescription=TRUE then this text is displayed in the About dialog
    property Description: TStrings read fDescriptionStringList write SetStrings;
    // Double-click or click the ellipsis button
    property About: tSplashAboutString read fAboutString;
    // If shaped form then this property is ignored
    property FormSplashWidth: integer read fFormSplashWidth
      write fFormSplashWidth default C_DEFAULTSPLASHWIDTH;
    // If shaped form then this property is ignored
    property FormSplashHeight: integer read fFormSplashHeight
      write fFormSplashHeight default C_DEFAULTSPLASHHEIGHT;
    // Use default value for the best layout
    property FormAboutWidth: integer read fFormAboutWidth
      write fFormAboutWidth default C_DEFAULTSPLASHWIDTH;
    // Use default value for the best layout
    property FormAboutHeight: integer read fFormAboutHeight
      write fFormAboutHeight default C_DEFAULTSPLASHHEIGHT;
    // Default is taCenter
    property FormSplashTextAlign: TAlignment
      read fFormSplashTextAlign write fFormSplashTextAlign default taCenter;
    // Default is taCenter
    property FormAboutTextAlign: TAlignment read fFormAboutTextAlign
      write fFormAboutTextAlign default taCenter;
    // Version number of SplashAbout source code
    property Version: string read fVersionString;
    {$IFNDEF LINUX}
    // If TRUE then ShowSplash is preceded by a 'Powered by Lazarus' splash
    property ShowPoweredBy: boolean
      read fShowPoweredBy write fShowPoweredBy default False; platform;
    {$ENDIF}
    // String list for scrolling credits
    property CreditLines: TStrings read fCreditLines write SetCreditLines;
    // Show the Credits button?
    property ShowCreditButton: boolean read fShowCreditButton write fShowCreditButton;
    // If this text file is present in the executable folder the contents will be used
    // to populate the scrolling Credits window
    property CreditsTextfileName: string read fCreditsTextfileName
      write fCreditsTextfileName;
    // Affects how the background bitmap behaves
    property ResizeMode: TResizeMode read fResizeMode write fResizeMode default
      saResizeGraphic;
    property PoweredBy:TPoweredBy read fPoweredBy write fPoweredBy;
  end;

  TAboutPropertyEditor = class(TClassPropertyEditor)
    // Custom property editor for 'About' property
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
  end;

procedure Register;

implementation

procedure Register;
// Called when installing component into the Lazarus IDE
begin
  {$I splashabout_ico.lrs}
  // Only use for the unique type 'tSplashAboutString'
  RegisterPropertyEditor(TypeInfo(tSplashAboutString),
    TSplashAbout, 'About', TAboutPropertyEditor);
  // Register subcomponent class property editor
  RegisterPropertyEditor(TypeInfo(TPoweredBy),
    TSplashAbout, 'PoweredBy', TClassPropertyEditor);

  RegisterComponents('Additional', [TSplashAbout]);
end;

// == START PROPERTY EDITOR CODE FOR About PROPERTY ==
procedure TAboutPropertyEditor.Edit;
// Shows a dialog when About property is double-clicked
var
  tAboutForm: TForm;
  OKbutton: TBitBtn;
  lbl_Description: TLabel;
  sz: string;
begin
  sz := 'SplashAbout component for Lazarus' + LineEnding +
    'by minesadorada@charcodelvalle.com' + LineEnding + LineEnding;
  sz += 'Methods:' + LineEnding;
  sz += 'SplashAbout1.ShowSplash' + LineEnding;
  sz += 'SplashAbout1.ShowAbout' + LineEnding;
  sz += LineEnding + 'Version: ' + C_SPLASHABOUT_VERSION + LineEnding;
  sz += 'License: LGPL';
  tAboutForm := TForm.CreateNew(nil);
  try  //.. finally FreeAndNil everything
    with tAboutForm do
    begin
      // Form
      position := poScreenCenter;
      borderstyle := bsToolWindow;
      Caption := 'About SplashAbout Component';
      formstyle := fsSystemStayOnTop;
      color := clSkyBlue;
      Height := 240;
      Width := 320;
      okbutton := TBitBtn.Create(tAboutForm);
      okbutton.Kind := bkClose;
      okbutton.left := (Width div 2) - okbutton.Width div 2;
      okbutton.top := Height - okbutton.Height - 10;
      okbutton.parent := tAboutForm;
      lbl_Description := Tlabel.Create(tAboutForm);
      lbl_Description.left := 8;
      lbl_Description.Top := 30;
      lbl_Description.Width := 304;
      lbl_Description.Height := 200;
      lbl_Description.Autosize := False;
      lbl_Description.Alignment := taCenter;
      lbl_Description.Caption := sz;
      lbl_Description.parent := tAboutForm;
      ShowModal;
    end;
  finally
    FreeAndNil(lbl_Description);
    FreeAndNil(okbutton);
    FreeAndNil(tAboutForm);
  end;
end;

function TAboutPropertyEditor.GetAttributes: TPropertyAttributes;
  // Show the ellipsis
begin
  Result := [paDialog, paReadOnly];
end;

function TAboutPropertyEditor.GetValue: string;
  // Override standard string read method
begin
  Result := '(About)';
end;
// == END PROPERTY EDITOR CODE FOR About PROPERTY ==


destructor TSplashAbout.Destroy;
begin
  Application.RemoveOnIdleHandler(@ApplicationOnIdle);
  FreeAndNil(fBackGroundBitmap);
  FreeAndNil(fMaskMonoBitmap);
  FreeAndNil(fMaskColorBitmap);
  FreeAndNil(fIcon);
  FreeAndNil(fDescriptionStringList);
  FreeAndNil(fFont);
  FreeAndNil(fCreditLines);
  inherited Destroy;
end;

constructor TSplashAbout.Create(AOwner: TComponent);
  // Initialise private vars
var
  Frm: TForm;
begin
  If (AOwner=Nil) then
  begin
    raise Exception.CreateFmt('%s : Sorry this component cannot be created in code',[C_SPLASHABOUT_ERROR]);
    Exit;
  end;
  inherited Create(AOwner);

  // From main form [Create(Self)]
  if (AOwner is TForm) then
  begin
    Frm := AOwner as TForm;
    fFormTitleString := Frm.Caption;
    fFormIcon := Frm.Icon;
  end;
  DelaySeconds := 2;
  BackGroundColor := clSkyBlue;
  fAppVersionString := LineEnding;
  // Use uversion unit public methods to populate (Credit: Mike Thompson)
  if (GetFileVersion <> 'No build information available') then
    fAppVersionString += 'Version ' + GetFileVersion + LineEnding + LineEnding;
  fVersionInfoString := fAppVersionString + 'Made with: ' + GetLCLVersion;
  fVersionInfoString += ' and ' + GetCompilerInfo + LineEnding;
  fVersionInfoString += 'For: ' + GetTargetInfo + ' (' + GetWidgetSet + ')' + LineEnding;
  fVersionInfoString += 'Last Compiled: ' + GetCompiledDate;

  // Optional property values
  fExternalFileBackGroundImage := '';
  fExternalFileMaskMonoImage := '';
  fTitleStyle := saApplication;
  fTitleString := 'My Application';
  fCloseOnIdle := False;
  fExternalFileOptions := [saExternalBackground, saExternalIcon,
    saExternalMaskColorImage, saExternalMaskMonoImage];
  fGraphicType := saResources;
  // Initialise property resources
  fBackGroundBitmap := TBitMap.Create;
  fMaskMonoBitmap := TBitMap.Create;
  fMaskColorBitmap := TBitMap.Create;
  fDescriptionStringList := TStringList.Create;
  fIcon := TIcon.Create;
  fFont := TFont.Create;
  fFont.Size := 10;
  fFormSplashWidth := C_DEFAULTSPLASHWIDTH;
  fFormSplashHeight := C_DEFAULTSPLASHHEIGHT;
  fFormAboutWidth := C_DEFAULTSPLASHWIDTH;
  fFormAboutHeight := C_DEFAULTSPLASHHEIGHT;
  {$IFDEF LINUX}
  fFormSplashHeight := C_DEFAULTSPLASHHEIGHT_LINUX;
  {$ENDIF}
  fFormSplashTextAlign := taCenter;
  fFormAboutTextAlign := taCenter;
  fVersionString := C_SPLASHABOUT_VERSION;
  fCreditLines := TStringList.Create;
  fCreditsTextfileName := 'credits.txt';
  fResizeMode := saResizeGraphic;

  // Use tPoweredBy as a subcomponent
  // Register a TClassPropertyEditor in order to display it correctly
  fPoweredBy := TPoweredBy.Create(Self);
  fPoweredBy.SetSubComponent(true);  // Tell the IDE to store the modified properties
  fPoweredBy.Name:='PoweredBy';
end;

procedure TSplashAbout.ApplicationOnIdle(Sender: TObject; var Done: boolean);
begin
  if fSplashForm = nil then
    exit;
  if fCloseOnIdle then
    if fSplashForm.Visible then
    begin
      fSplashForm.ModalResult := 11;//mrClose;
      Done := True;
    end;
end;

function TSplashAbout.MakeBMPfromJPG(var JPGFilePath: string): boolean;
  // The BitmapMaskMono has to be a BMP image file (ideally 2bbp).
  // If the developer sets the property to a JPG image file, then this routine
  // will convert it to BMP and save it to disk.  The resulting BMP file is likely
  // to be a huge 24bbp file!
  // WARNING! The routine overwrites any existing BMP with the name name as the old JPG
var
  JPG: TJpegImage;
  BMP: TBitmap;
  s: string;
begin
  Result := False;
  if not FileExists(JPGFilePath) then
    exit;
  try
    JPG := TJpegImage.Create;
    BMP := TBitmap.Create;
    try
      JPG.LoadFromFile(JPGFilePath);
      BMP.Assign(jpg);
      s := ChangeFileExt(JPGFilePath, '.bmp');
      if FileExists(s) then
        ShowMessageFmt('Over-writing old %s', [s]);
      BMP.SaveToFile(s);
      if FileExists(s) then
      begin
        JPGFilePath := s;
        Result := True;
      end;
    except
      On E: Exception do
        MessageDlg(C_SPLASHABOUT_ERROR,
          'There is something wrong with the MaskImage JPG File' +
          LineEnding + 'Error detail: ' + e.message,
          mtError, [mbOK], 0);
    end;
  finally
    BMP.Free;
    JPG.Free;
  end;
end;

procedure TSplashAbout.ShowCredits(Sender: TObject);
var
  CreditsForm: TForm;
  CloseButton: TBitBtn;
  ScrollWindow: TScrollTextClass;

begin
  // Create a window, scroller and close button on-the-fly
  CreditsForm := TForm.Create(nil);
  CloseButton := TBitBtn.Create(CreditsForm);
  ScrollWindow := TScrollTextClass.Create(CreditsForm);
  try
    with CreditsForm do
    begin
      position := poScreenCenter;
      borderstyle := bsToolWindow;
      Caption := fTitleString + ': Credits';
      formstyle := fsSystemStayOnTop;
      Color := clBlack; // For a black border
      // Use the same window size as the License window
      {$IFDEF WINDOWS}
      // More compact GUI?
      Width := C_DEFAULTLICENSEFORMWIDTH;
      Height := C_DEFAULTLICENSEFORMHEIGHT;
      {$ELSE WINDOWS}
      Width := C_DEFAULTLICENSEFORMWIDTH_LINUX;
      Height := C_DEFAULTLICENSEFORMHEIGHT_LINUX;
      {$ENDIF}


      // Close Button
      CloseButton.Kind := bkClose;
      CloseButton.left := (Width div 2) - CloseButton.Width div 2;
      CloseButton.top := Height - CloseButton.Height - 10;
      CloseButton.parent := CreditsForm;

      // Scroller panel
      ScrollWindow.Parent := CreditsForm;
      ScrollWindow.Align := alClient;
      ScrollWindow.BorderSpacing.Around := 2;
      ScrollWindow.BackColor := fBackGroundColor;
      ScrollWindow.Font.Assign(fFont);

      // Load from string list?
      if CreditLines.Count > 0 then
        ScrollWindow.Lines.Assign(CreditLines);

      // Load from CreditsTextFile if present
      // This would overwrite any text in CreditLines
      if FileExists(fCreditsTextfileName) then
        ScrollWindow.Lines.LoadFromFile(fCreditsTextfileName);

      // If both are empty then default text is shown.
      ScrollWindow.Active := True;
      // Show modally over the existing modal form
      PopupParent := TForm(Sender);
      ShowModal;
    end;
  finally
    CloseButton.Free;
    ScrollWindow.Free;
    CreditsForm.Free;
  end;
end;

procedure TSplashAbout.ShowLicense(Sender: TObject);
// Triggered by License button Click
var
  sLicenseString: string;
  theList: TStringList;
  f: integer;
  LicenceForm: TForm;
  lblText: TLabel;
  closebuttton: TBitBtn;
  r: TLResource;
begin
  // Set to resource name in license.lrs
  case fLicenseFile of
    saNone: szLicenseFile := '';
    saGPL: szLicenseFile := 'gpl.txt';
    saLGPL: szLicenseFile := 'lgpl.txt';
    saMIT: szLicenseFile := 'mit.txt';
    saModifiedgpl: szLicenseFile := 'modifiedgpl.txt';
    saUserFile: szLicenseFile := 'license.txt';
  end;
  if fLicenseFile = saNone then
    Exit;

  // Use a string list to split the text file into lines
  theList := TStringList.Create;
  // Create a window, label and close button on-the-fly
  LicenceForm := TForm.Create(nil);
  lblText := TLabel.Create(LicenceForm);
  closebuttton := TBitBtn.Create(LicenceForm);
  // Load up the text into variable 'sLicenseString'
  sLicenseString := LineEnding + LineEnding + fTitleString + LineEnding;
  try
    try
      if (fLicenseFile = saUserFile) then
      begin
        theList.LoadFromFile(szLicenseFile);
        sLicenseString += LineEnding;
        if (theList.Count = 0) then
          raise Exception.CreateFmt('%s does not contain any text', [szLicenseFile]);
        for f := 0 to TheList.Count - 1 do
          sLicenseString += TheList[f] + LineEnding;
      end
      else
      begin
        // Load license text from resource string
        r := LazarusResources.Find(szLicenseFile);
        if r = nil then
          raise Exception.Create('Resource datafile license.lrs is missing');
        thelist.Add(r.Value);
        for f := 0 to TheList.Count - 1 do
          sLicenseString += TheList[f] + LineEnding;
      end;
    except
      On e: Exception do
        MessageDlg(C_SPLASHABOUT_ERROR,
          'There is something wrong with the Licence text', mtError, [mbOK], 0);
    end;

    // Replace boilerplate text if possible
    sLicenseString := AnsiReplaceText(sLicenseString, '<year>',
{$I %DATE%}
      );
    sLicenseString := AnsiReplaceText(sLicenseString, '<name of author>', fAuthorString);
    sLicenseString := AnsiReplaceText(sLicenseString, '<contact>',
      '(' + fSupportContactString + ')');
    sLicenseString := AnsiReplaceText(sLicenseString, '<copyright holders>',
      fAuthorString);

    // Make up the form window and controls
    with LicenceForm do
    begin
      // Form
      {$IFDEF WINDOWS}
      // More compact GUI?
      Width := C_DEFAULTLICENSEFORMWIDTH;
      Height := C_DEFAULTLICENSEFORMHEIGHT;
      {$ELSE WINDOWS}
      Width := C_DEFAULTLICENSEFORMWIDTH_LINUX;
      Height := C_DEFAULTLICENSEFORMHEIGHT_LINUX;
      {$ENDIF}
      // autosize:=true;
      // If you enable autosize, the button placement goes awry!

      // The Modified GPL has an extra clause
      if (szLicenseFile = 'modifiedgpl.txt') or
        (Pos('As a special exception', sLicenseString) > 0) then
        Height := Height + 100;
      position := poScreenCenter;
      borderstyle := bsToolWindow;
      Caption := fTitleString + ': Licensing';
      formstyle := fsSystemStayOnTop;

      // Label
      lblText.Align := alClient;
      lblText.Alignment := taCenter;
      lblText.Caption := sLicenseString;
      lblText.Parent := LicenceForm;

      // Close Button
      closebuttton.Kind := bkClose;
      closebuttton.left := (Width div 2) - closebuttton.Width div 2;
      closebuttton.top := Height - closebuttton.Height - 10;
      closebuttton.parent := LicenceForm;
      // Show modally over the existing modal form
      PopupParent := TForm(Sender);
      ShowModal;
    end;
  finally
    // Free up all component created resources from memory
    FreeAndNil(theList);
    FreeAndNil(lblText);
    FreeAndNil(closebuttton);
    FreeAndNil(LicenceForm);
  end;
end;

procedure TSplashAbout.CloseForm(Sender: TObject);
// Triggered by a Timer.OnTimer event or CloseButton.Click or OnClick
begin
  fSplashForm.Close; // Hide and destroy
end;

procedure TSplashAbout.ShowSplash;
// Set the mode, then create and show the form
begin
  fFormType := fSplash;
  ShowForm;
end;

procedure TSplashAbout.ShowAbout;
// Set the mode, then create and show the form
begin
  fFormType := fAbout;
  ShowForm;
end;

procedure TSplashAbout.ShowForm;
// Main method
// Construct a form and show it modally
// Controls vary according to fFormType variable
const
  TEXT_MARGIN = 30; // Pixels from left and right
var
  OKButton, LicenseButton, CreditsButton: TBitBtn;
  Delaytimer, scrolltimer: TTimer;
  lbl_Title, lbl_VersionInfo: TLabel;
  img_icon, img_background: TImage;
  bevel: TBevel;
  MyBitMap: TBitMap;
  FormRect: TRect;
  sVersionInfoString: string;
  iFormHeight, iFormWidth: integer;
  szMaskName: string;
  bUsingResourceMask, bMaskPSplash: boolean;
  iCount: integer;
  bLicenseButtonVisible, bCreditsButtonVisible: boolean;

label
  NOTEXT; // Used in a GoTo

begin

  // Show 'Powered by Lazarus?'
  if ((fShowPoweredBy = True) and ((fFormType = fSplash))) then
    fPoweredBy.ShowPoweredByForm;

  bMaskPSplash := False; // Toggle used in background image code
  bUsingResourceMask := False; // True if saRoundedRect etc are used
  //Establish License
  case fLicenseFile of
    saNone:
    begin
      szLicenseFile := '';
      fLicenseTypeString := '';
    end;
    saGPL:
    begin
      szLicenseFile := 'gpl.txt';
      fLicenseTypeString := 'GPL License';
    end;
    saLGPL:
    begin
      szLicenseFile := 'lgpl.txt';
      fLicenseTypeString := 'Library GPL License';
    end;
    saMIT:
    begin
      szLicenseFile := 'mit.txt';
      fLicenseTypeString := 'MIT License';
    end;
    saModifiedgpl:
    begin
      szLicenseFile := 'modifiedgpl.txt';
      fLicenseTypeString := 'Modified GPL License';
    end;
    saUserFile:
    begin
      szLicenseFile := 'license.txt';
      fLicenseTypeString := 'Proprietry License';
    end;
  end;
  // Set resource name if appropriate, else filename of user image
  case fMaskType of
    saNoMask: szMaskName := 'none';
    saUserImage: szMaskName := fExternalFileMaskMonoImage;
    saRoundedRect: szMaskName := 'roundedrect';
    saBigCog: szMaskName := 'bigcog';
    saBigFlower: szMaskName := 'bigflower';
    saBigSplash: szMaskName := 'bigsplash';
  end;

  // Temporarily create the form and controls
  fSplashForm := TForm.CreateNew(nil);
  if (fTitleStyle = saApplication) then
    fTitleString := Application.Title;
  if (fTitleStyle = saForm) then
    fTitleString := fFormTitleString;

  fSplashForm.Font := fFont;
  // The created form is parent to all the controls
  bevel := TBevel.Create(fSplashForm);
  OKButton := TBitBtn.Create(fSplashForm);
  LicenseButton := TBitBtn.Create(fSplashForm);
  CreditsButton := TBitBtn.Create(fSplashForm);
  Delaytimer := TTimer.Create(fSplashForm);
  Scrolltimer := TTimer.Create(fSplashForm);
  lbl_Title := TLabel.Create(fSplashForm);
  lbl_VersionInfo := TLabel.Create(fSplashForm);
  img_icon := TImage.Create(fSplashForm);
  img_background := TImage.Create(fSplashForm);
  MyBitmap := TBitMap.Create;
  iFormHeight := fFormSplashHeight;
  iFormWidth := fFormSplashWidth;
  if fFormType = fAbout then
  begin
    iFormHeight := fFormAboutHeight;
    iFormWidth := fFormAboutWidth;
  end;
  lbl_Title.ParentFont := True;
  lbl_VersionInfo.ParentFont := True;

  // Now set positions and properties
  try  //.. finally FreeAndNil everything
    with fSplashForm do
    begin
      // Form
      position := poScreenCenter;
      if fFormType = fAbout then
        borderstyle := bsToolWindow
      else
        borderstyle := bsnone;
      Caption := 'About ' + fTitleString;
      formstyle := fsSystemStayOnTop;
      color := fBackGroundColor;
      // Set constraints so that if ResizeMode=ResizeWindow
      // then the form can't disappear nor overwhelm the screen
      Constraints.MaxWidth := 1024;
      Constraints.MaxHeight := 768;
      Constraints.MinWidth := 320;
      Constraints.MinHeight := 240;
      Height := iFormHeight;
      Width := iFormWidth;



      // Shaped form?
      // Form is sized to mask image (MUST be BMP file)
      // Text is centred in a 320 x 240 invisible frame
      {$IFNDEF LINUX}// Problem with Canvas.Draw in linux!

      // Skip this mask code block if MaskType is saNoMask
      if (fFormType = fSplash) and (fMaskType <> saNoMask) then
      begin
        // Deal with user-supplied image first
        if (fMaskType = saUserImage) then

          if ((fGraphicType = saExternalFiles) and
            (saExternalMaskMonoImage in fExternalFileOptions)) then
            if (FileExists(fExternalFileMaskMonoImage)) then
            begin
              // Try to convert a jpg file if specified as such
              if ExtractFileExt(fExternalFileMaskMonoImage) = '.jpg' then
                if MakeBMPfromJPG(fExternalFileMaskMonoImage) = False then
                  MessageDlg(C_SPLASHABOUT_ERROR,
                    'There is something wrong with the MaskImage or MaskPImage File',
                    mtError, [mbOK], 0)
                else
                  MessageDlg(C_SPLASHABOUT_ERROR,
                    'The MaskImage should be a .BMP file.  Your jpg has been converted and saved as a bmp.  Please amend the property.',
                    mtInformation, [mbOK], 0);
              // Load the user image into the BitMap
              MyBitMap.LoadFromFile(fExternalFileMaskMonoImage);
              //MyBitmap.SetSize(iFormWidth,iFormHeight);
            end
            else
              // Looks like the specified user image isn't in the application folder
            begin
              MessageDlg(C_SPLASHABOUT_ERROR,
                'Cannot find MaskImage ' + fExternalFileMaskMonoImage,
                mtError, [mbOK], 0);
              exit;
            end;

        // Deal with Bitmap resource
        if (fGraphicType = saResources) then
          if Assigned(fMaskMonoBitmap) then
          begin
            MyBitMap.Assign(fMaskMonoBitmap);
            // MyBitmap.SetSize(iFormWidth,iFormHeight);
          end;

        // Stock image specified, so load from masks.lrs
        if (fMaskType <> saUserImage) then
        begin
          MyBitmap.LoadFromLazarusResource(szMaskName);
          bUsingResourceMask := True;
        end;

        // Now to use the loaded BitMap
        try
          // ShowMessageFmt('MyBitMap.Height = %d,MyBitMap.Width=%d',[MyBitMap.Height,MyBitMap.Width]);
          if MyBitMap.Height >= iFormHeight then
            iFormHeight := MyBitMap.Height;
          if MyBitMap.Width >= iFormWidth then
            iFormWidth := MyBitMap.Width;

          MyBitMap.Transparent := True;
          MyBitMap.TransparentColor := clBlack;
          Height := iFormHeight;
          Width := iFormWidth;
          FormRect.Top := 0;
          FormRect.left := 0;
          FormRect.Right := Width;
          FormRect.Bottom := Height;
          // Use StretchDraw
          Canvas.StretchDraw(FormRect, MyBitmap);
          // Canvas.Draw(0, 0, MyBitMap);
          // raises Floating Point Error in 64-bit Nix (!??)
          SetShape(MyBitMap);
          bevel.Visible := False;
        except
          On e: Exception do
            MessageDlg(C_SPLASHABOUT_ERROR,
              'There is something wrong with the MaskMonoImage or MaskColourImage File' +
              LineEnding + 'Error detail: ' + e.message,
              mtError, [mbOK], 0);
        end;
      end;
{$ENDIF}

      // Delay Timer
      if fFormType = fSplash then
      begin
        if fDelaySeconds = 0 then
          fCloseOnIdle := True
        else
          fCloseOnIdle := False;
        if FCloseOnIdle then
          Application.AddOnIdleHandler(@ApplicationOnIdle)
        else
        begin
          // Fix negative values
          if fDelaySeconds < 1 then
            fDelaySeconds := 1;
          // Fix developer mistakenly specifying milliseconds
          if fDelaySeconds > 1000 then
            fDelaySeconds := fDelaySeconds div 1000;
          delaytimer.Interval := fDelaySeconds * 1000;
          delaytimer.OnTimer := @CloseForm;
          delaytimer.Enabled := True;
        end;
      end;

      // bevel
      // Controls are placed relative to the bevel window
      if fFormType = fAbout then
      begin
        bevel.Width := fFormAboutWidth;
        bevel.Height := fFormAboutHeight;
      end
      else
      begin
        bevel.Width := fFormSplashWidth;
        bevel.Height := fFormSplashHeight;
      end;
      if iFormHeight > bevel.Height then
        bevel.Top := (iFormHeight - bevel.Height) div 2
      else
        bevel.Top := 0;
      if iFormWidth > bevel.Width then
        bevel.Left := (iFormWidth - bevel.Width) div 2
      else
        bevel.Left := 0;
      bevel.BorderSpacing.Around := 4;
      bevel.BorderSpacing.InnerBorder := 4;
      bevel.Parent := fSplashForm;
      bevel.onClick := @CloseForm;

      // BackGround image - load from file or resource
      if ((fResizeMode = saResizeGraphic) or (bUsingResourceMask = True)) then
        img_background.Stretch := True
      else
        img_background.Stretch := False;

      img_background.Align := alClient;
      img_background.Parent := fSplashForm;
      img_background.SendToBack;

      if ((fGraphicType = saExternalFiles) and
        (FileExists(fExternalFileBackGroundImage))) or
        ((fGraphicType = saResources) and (Assigned(fBackGroundBitmap))) then
      begin
        try
          if ((fGraphicType = saExternalFiles) and
            (saExternalBackground in fExternalFileOptions)) then
            img_background.Picture.LoadFromFile(fExternalFileBackGroundImage);
          if (fGraphicType = saResources) then
            img_background.Picture.Assign(fBackGroundBitmap);


          if (fResizeMode = saResizeWindow) then
          begin
            if (fGraphicType = saResources) then
            begin
              // Expand the form to suit the image
              iFormHeight := fBackGroundBitmap.Height;
              iFormWidth := fBackGroundBitmap.Width;
            end
            else
            begin
              // Expand the form to suit the image
              iFormHeight := img_background.Height;
              iFormWidth := img_background.Width;
            end;
            Height := iFormHeight;
            Width := iFormWidth;
            bevel.Align := alClient;
          end;

        except
          On e: Exception do
            MessageDlg(C_SPLASHABOUT_ERROR,
              'There is something wrong with the Background Image', mtError, [mbOK], 0);
        end;
      end;


      // In splash screen override BackGroundImage with MaskColorImage?
      if (fFormType = fSplash) and (fMaskType = saUserImage) then
      begin
        if (fGraphicType = saExternalFiles) then
          // Check images exist
          if (FileExists(fExternalFileMaskColorImage)) and
            (FileExists(fExternalFileMaskMonoImage)) and
            (saExternalMaskColorImage in fExternalFileOptions) and
            (saExternalMaskMonoImage in fExternalFileOptions) then
          begin
            try
              img_background.Picture.LoadFromFile(fExternalFileMaskColorImage);
              bMaskPSplash := True;
              // Triggers GoTo statement that skips text and icon elements
            except
              On e: Exception do
                MessageDlg(C_SPLASHABOUT_ERROR,
                  'There is something wrong with the External mask color Image',
                  mtError, [mbOK], 0);
            end;
          end;
        if (fGraphicType = saResources) and Assigned(fMaskColorBitmap) then
        begin
          img_background.Picture.Assign(fMaskColorBitmap);
          bMaskPSplash := True;
        end;
      end;
      if bMaskPSplash then
      begin
        // Mask images should not resize the form
        img_background.Stretch := True;
        Height := iFormHeight;
        Width := iFormWidth;
        bevel.Align := alClient;
      end;
      // If a shaped mask with image is the splash screen then don't display any text or icon
      if bMaskPSplash then
        goto NOTEXT;

      // Title
      if fFormType = fSplash then
      begin
        lbl_Title.Top := bevel.Top + 64;
        lbl_Title.Left := bevel.Left + TEXT_MARGIN;
        lbl_Title.AutoSize := False;
        lbl_Title.Width := bevel.Width - (TEXT_MARGIN * 2);
        lbl_Title.Font := fFont;
        lbl_Title.Font.Style := [fsBold];
        lbl_Title.Font.Size := 12;
        lbl_Title.Height := 32;
        lbl_Title.Alignment := fFormSplashTextAlign;
        if (fTitleString = '') then
          lbl_Title.Caption := Application.Title
        else
          lbl_Title.Caption := fTitleString;
        lbl_Title.Parent := fSplashForm;
      end;


      // Icon
      // Icon can come from: Icon property, External file, Form or Application
      img_icon.Width := 32;
      img_icon.Height := 32;
      img_icon.Top := bevel.Top + 20;
      img_icon.Left := bevel.left + ((bevel.Width - 32) - 20);
      img_icon.Stretch := True;
      img_icon.Parent := fSplashForm;

      // Fallback is to the Application icon, then Form icon, then nil.
      try
        // A new Application is always assigned a default icon
        if (fTitleStyle = saApplication) then
          if Assigned(Application.Icon) then
            img_icon.Picture.Icon := Application.Icon;

        // The Form may not have an icon
        if (fTitleStyle = saForm) then
          if (fFormIcon = nil) then
            img_icon.Picture.Icon := fFormIcon
          else
          if Assigned(Application.Icon) then
            img_icon.Picture.Icon := Application.Icon;

        // Fall back to application and form icons if necessary
        if (fTitleStyle = saUserTitle) then
        begin
          if ((fGraphicType = saExternalFiles) and
            (saExternalIcon in fExternalFileOptions)) then
          begin
            if FileExists(fExternalFileIcon) then
            begin
              fIcon.LoadFromFile(fExternalFileIcon);
              img_icon.Picture.Icon := fIcon;
            end
            else
            if (fFormIcon <> nil) then
              img_icon.Picture.Icon := fFormIcon
            else
            if Assigned(Application.Icon) then
              img_icon.Picture.Icon := Application.Icon;
          end;


          if (fGraphicType = saResources) then
            if Assigned(fIcon) then
              img_icon.Picture.Icon := fIcon
            else
            if (fFormIcon <> nil) then
              img_icon.Picture.Icon := fFormIcon
            else
            if Assigned(Application.Icon) then
              img_icon.Picture.Icon := Application.Icon;
        end;

      except
        On e: Exception do
          MessageDlg(C_SPLASHABOUT_ERROR, 'There is something wrong with the Icon',
            mtError, [mbOK], 0);
      end;

      // About dialog buttons
      // For code clarity, use these flags
      bCreditsButtonVisible := False;
      bLicenseButtonVisible := False;
      // Close Button (positioned in the centre)
      if fFormType = fAbout then
      begin
        OKButton.Kind := bkClose;
        OKButton.left := (Width div 2) - OKButton.Width div 2;
        OKButton.top := Height - OKButton.Height - 10;
        OKButton.parent := fSplashForm;
        // Do we need to show the Credits and/or License buttons?
        if (fShowCreditButton = True) then
          bCreditsButtonVisible := True;
        // License Button
        // Don't show if LicenseFile=saNone or LicenseFile=saUserText and 'license.txt' is missing
        if (fLicenseFile <> saUserFile) or
          ((fLicenseFile = saUserFile) and (FileExists(szLicenseFile))) then
          if (fLicenseFile <> saNone) then
            bLicenseButtonVisible := True;
        // License button
        if bLicenseButtonVisible then
          // Put it on the right
        begin
          LicenseButton.Top := OKButton.Top;
          LicenseButton.Caption := 'License...';
          LicenseButton.left := Width - LicenseButton.Width - 10;
          LicenseButton.OnClick := @ShowLicense;
          LicenseButton.Parent := fSplashForm;
        end;
        // Credits button
        if bCreditsButtonVisible then
          // Put it on the right
        begin
          CreditsButton.Kind := bkCustom;
          CreditsButton.top := OKButton.top;
          CreditsButton.Left := Width - CreditsButton.Width - 10;
          ;
          CreditsButton.Caption := 'Credits..';
          CreditsButton.OnClick := @ShowCredits;
          CreditsButton.parent := fSplashForm;
        end;
        // Reposition the various buttons?
        if (bLicenseButtonVisible and bCreditsButtonVisible) then
        begin
          CreditsButton.Left := LicenseButton.Left - CreditsButton.Width;
          OKButton.left := CreditsButton.Left - OKButton.Width;
        end;
      end;


      // Version Info or Description
      lbl_VersionInfo.Autosize := False;
      lbl_VersionInfo.WordWrap := True;
      lbl_VersionInfo.Left := bevel.left + TEXT_MARGIN;
      lbl_VersionInfo.Width := bevel.Width - TEXT_MARGIN * 2;
      lbl_VersionInfo.Height := bevel.Height - 50;
      if fFormType = fAbout then
        lbl_VersionInfo.Alignment := fFormAboutTextAlign
      else
        lbl_VersionInfo.Alignment := fFormSplashTextAlign;

      sVersionInfoString := '';
      if fFormType = fAbout then
        sVersionInfoString += fTitleString;
      if fOrganisationString <> '' then
        sVersionInfoString += LineEnding + fOrganisationString;

      // Show description or VersionInfo?
      if (fShowDescription = True) then
      begin
        sVersionInfoString += LineEnding + fAppVersionString;
        if (fDescriptionStringList.Count > 0) then
          for iCount := 0 to fDescriptionStringList.Count - 1 do
          begin
            sVersionInfoString += fDescriptionStringList[iCount] + LineEnding;
          end
        else
          // User has not set the Description property
          sVersionInfoString += 'No Description';
      end
      else
        sVersionInfoString += LineEnding + fVersionInfoString;

      if fLicenseTypeString <> '' then
        sVersionInfoString += LineEnding + LineEnding + 'Released under ' +
          fLicenseTypeString;
      lbl_VersionInfo.Caption := sVersionInfoString;
      if fFormType = fSplash then
        lbl_VersionInfo.Top :=
          Bevel.Top + (bevel.Height div 2) - 50
      else
        lbl_VersionInfo.Top := 40;
      lbl_VersionInfo.Parent := fSplashForm;
      lbl_VersionInfo.onClick := @CloseForm;

      NOTEXT:

        // Now show the completed form
        Application.ProcessMessages;
      ShowModal;
    end;
  finally
    // Controls normally destroyed with parent
    // but if Try block fails, ensure no memory leaks
    FreeAndNil(bevel);
    FreeAndNil(img_icon);
    FreeAndNil(img_background);
    FreeAndNil(lbl_Title);
    FreeAndNil(lbl_VersionInfo);
    FreeAndNil(OKButton);
    FreeAndNil(LicenseButton);
    FreeAndNil(CreditsButton);
    FreeAndNil(delaytimer);
    FreeAndNil(Scrolltimer);
    FreeAndNil(MyBitMap);
    FreeAndNil(fSplashForm);
  end;
end;
// Various Set property routines
// Note that the object properties are Assigned to
procedure TSplashAbout.SetBackGroundBitmap(const AValue: TBitmap);
begin
  fBackGroundBitmap.Assign(AValue);
end;

procedure TSplashAbout.SetMaskMonoBitmap(const AValue: TBitmap);
begin
  fMaskMonoBitmap.Assign(AValue);
end;

procedure TSplashAbout.SetMaskColorBitmap(const AValue: TBitmap);
begin
  fMaskColorBitmap.Assign(AValue);
end;

procedure TSplashAbout.SetStrings(const AValue: TStrings);
begin
  fDescriptionStringList.Assign(AValue);
end;

procedure TSplashAbout.SetFont(const AValue: TFont);
begin
  fFont.Assign(AValue);
end;

procedure TSplashAbout.SetCreditLines(const AValue: TStrings);
begin
  fCreditlines.Assign(AValue);
end;

initialization
  // Load license text and masks as lazarus resources
{$I license.lrs}
{$I masks.lrs}
{$Hint Don't worry about the 'Warning: Symbol xxx is not portable' messages}
{$Hint The 'not portable' code is stuff that won't work outside of Windows}
{$Hint ..so I flagged it as such in the component code}

end.
