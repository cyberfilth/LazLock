{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)

  Copyright (C) 2013-2020 Vojtěch Čihák, Czech Republic

  Credit: alignment of composite components (classes TECEditBtnSpacing and TECComboBtnSpacing)
    is based on idea of Flávio Etrusco published on mailing list.
    http://lists.lazarus.freepascal.org/pipermail/lazarus/2013-March/079971.html

  This library is free software; you can redistribute it and/or modify it under the terms of the
  GNU Library General Public License as published by the Free Software Foundation; either version
  2 of the License, or (at your option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you permission to link this
  library with independent modules to produce an executable, regardless of the license terms of
  these independent modules,and to copy and distribute the resulting executable under terms of
  your choice, provided that you also meet, for each linked independent module, the terms and
  conditions of the license of that module. An independent module is a module which is not derived
  from or based on this library. If you modify this library, you may extend this exception to your
  version of the library, but you are not obligated to do so. If you do not wish to do so, delete
  this exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
  without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See
  the GNU Library General Public License for more details.

  You should have received a copy of the GNU Library General Public License along with this
  library; if not, write to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
  Boston, MA 02111-1307, USA.

**************************************************************************************************}

unit ECEditBtns;
{$mode objfpc}{$H+}  

//{$DEFINE DBGCTRLS}  {don't remove, just comment}

interface

uses
  Classes, SysUtils, Controls, Graphics, Math, Themes, Types, StdCtrls, ActnList,
  Dialogs, Forms, ImgList, LCLIntf, LCLProc, LCLType, LMessages, ECSpinCtrls, ECTypes;

type       
  {$PACKENUM 2}
  TButtonMode = (ebmButton, ebmToggleBox, ebmDelayBtn);
  TDropDownGlyph = (edgNone, edgMiddle, edgDown);
  TEBOption = (eboClickAltEnter, eboClickCtrlEnter, eboClickShiftEnter, eboInCellEditor);
  TEBOptions = set of TEBOption;   
  TItemOrder = (eioFixed, eioHistory, eioSorted);
  { event }
  TOnDrawGlyph = procedure(Sender: TObject; AState: TItemState) of object;
  
const
  cDefEBOptions = [eboClickAltEnter, eboClickCtrlEnter, eboClickShiftEnter];
  cDefBtnCheckedFontStyles = [];
  cDefBtnSpacing = 6;
  cBtnDropDownGlyphIndent: SmallInt = 4;
  cBtnMargin: SmallInt = 6;

type 
  TCustomECSpeedBtn = class;

  { TECSpeedBtnActionLink }
  TECSpeedBtnActionLink = class(TWinControlActionLink)
  protected
    FClientSpeedBtn: TCustomECSpeedBtn;
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  public
    function IsCheckedLinked: Boolean; override;
  end;

  TECSpeedBtnActionLinkClass = class of TECSpeedBtnActionLink;    
  
  { TCustomECSpeedBtn }
  TCustomECSpeedBtn = class(TGraphicControl)
  private
    FAllowAllUp: Boolean;
    FChecked: Boolean;
    FCheckedFontColor: TColor;
    FCheckedFontStyles: TFontStyles;
    FCheckFromAction: Boolean;
    FDelay: Integer;
    FDropDownGlyph: TDropDownGlyph;
    FFlat: Boolean;
    FGlyphColor: TColor;
    FGlyphDesignChecked: TGlyphDesign;
    FGlyphDesign: TGlyphDesign;
    FGroupIndex: Integer;
    FImageIndex: TImageIndex;
    FImageIndexChecked: TImageIndex;
    FImages: TCustomImageList;
    FImageWidth: SmallInt;
    FLayout: TObjectPos;
    FMargin: SmallInt;
    FMode: TButtonMode;
    FRepeating: Integer;
    FShowCaption: Boolean;
    FSpacing: SmallInt;
    FOnChange: TNotifyEvent;
    FOnDrawGlyph: TOnDrawGlyph;
    FOnHoldDown: TNotifyEvent;
    FOnRelease: TNotifyEvent;
    FOnRepeating: TNotifyEvent;
    FTransparent: Boolean;
    procedure SetAllowAllUp(AValue: Boolean);
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckedFontColor(AValue: TColor);
    procedure SetCheckedFontStyles(AValue: TFontStyles);
    procedure SetDelay(AValue: Integer);
    procedure SetDropDownGlyph(AValue: TDropDownGlyph);
    procedure SetFlat(AValue: Boolean);
    procedure SetGlyphColor(AValue: TColor);
    procedure SetGlyphDesign(AValue: TGlyphDesign);
    procedure SetGlyphDesignChecked(AValue: TGlyphDesign);
    procedure SetGroupIndex(AValue: Integer);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetImageIndexChecked(AValue: TImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImageWidth(AValue: SmallInt);
    procedure SetLayout(AValue: TObjectPos);
    procedure SetMargin(AValue: SmallInt);
    procedure SetMode(AValue: TButtonMode);
    procedure SetRepeating(AValue: Integer);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetSpacing(AValue: SmallInt);
    procedure SetTransparent(AValue: Boolean);
  protected const
    cDefHeight = 23;
    cDefWidth = 21;
  protected
    ActionListImagesDelay: Boolean;
    BtnDrawnPushed: Boolean;
    BtnPushed: Boolean;
    NeedRedraw: Boolean;
    PrevSize: TSize;
    RealLayout: TObjectPos;
    ECTimer: TCustomECTimer;
    ValidStates: TItemStates;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer; 
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure Click; override;
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMButtonPressed(var Message: TLMessage); message CM_BUTTONPRESSED;
    procedure CMColorChanged(var {%H-}Message: TLMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CreateTimer;
    function DialogChar(var Message: TLMKey): Boolean; override;
    procedure DrawButtonBMPs;
    procedure FontChanged(Sender: TObject); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function HasValidActImage: Boolean;
    function HasValidImages: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Redraw;
    procedure Resize; override;
    procedure ResizeInvalidate;
    procedure SetAction(Value: TBasicAction); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetTimerEvent;
    procedure TextChanged; override;
    procedure TimerOnTimerDelay(Sender: TObject);
    procedure TimerOnTimerHold(Sender: TObject);
    procedure TimerOnTimerRepeating(Sender: TObject);
    procedure UpdateGroup;
    property CheckFromAction: Boolean read FCheckFromAction write FCheckFromAction;
  public
    BtnBitmaps: array [TItemState] of TBitmap;
    UpdateCount: SmallInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Checked: Boolean read FChecked write SetChecked default False;
    property CheckedFontColor: TColor read FCheckedFontColor write SetCheckedFontColor default clDefault;
    property CheckedFontStyles: TFontStyles read FCheckedFontStyles write SetCheckedFontStyles default cDefBtnCheckedFontStyles;
    property Delay: Integer read FDelay write SetDelay default 0;
    property DropDownGlyph: TDropDownGlyph read FDropDownGlyph write SetDropDownGlyph default edgNone;
    property Flat: Boolean read FFlat write SetFlat default False;
    property GlyphColor: TColor read FGlyphColor write SetGlyphColor default clDefault;
    property GlyphDesign: TGlyphDesign read FGlyphDesign write SetGlyphDesign;  { set default in descendants }
    property GlyphDesignChecked: TGlyphDesign read FGlyphDesignChecked write SetGlyphDesignChecked default egdNone;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageIndexChecked: TImageIndex read FImageIndexChecked write SetImageIndexChecked default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageWidth: SmallInt read FImageWidth write SetImageWidth default 0;
    property Layout: TObjectPos read FLayout write SetLayout default eopLeft;
    property Margin: SmallInt read FMargin write SetMargin default -1;
    property Mode: TButtonMode read FMode write SetMode default ebmButton;
    property Repeating: Integer read FRepeating write SetRepeating default 0;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property Spacing: SmallInt read FSpacing write SetSpacing default cDefBtnSpacing;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawGlyph: TOnDrawGlyph read FOnDrawGlyph write FOnDrawGlyph;
    property OnHoldDown: TNotifyEvent read FOnHoldDown write FOnHoldDown;
    property OnRelease: TNotifyEvent read FOnRelease write FOnRelease;
    property OnRepeating: TNotifyEvent read FOnRepeating write FOnRepeating;
  end;

  { TECSpeedBtn }
  TECSpeedBtn = class(TCustomECSpeedBtn)
  published
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property CheckedFontColor;
    property CheckedFontStyles;
    {property Color;}  {does nothing ATM}
    property Constraints;
    property Delay;
    property DropDownGlyph;
    property Enabled;
    property Flat;
    property Font;
    property GlyphColor;
    property GlyphDesign default egdNone;
    property GlyphDesignChecked;
    property GroupIndex;
    property ImageIndex;
    property ImageIndexChecked;
    property Images;
    property ImageWidth;
    property Layout;
    property Margin;
    property Mode;
    property PopupMenu;
    property ParentBiDiMode;
    {property ParentColor;}  {does nothing ATM}
    property ParentFont;
    property ParentShowHint;
    property Repeating;
    property ShowCaption;
    property ShowHint;
    property Spacing;
    property Visible;
    property OnChangeBounds;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawGlyph;
    property OnHoldDown;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnRelease;
    property OnRepeating;
    property OnResize;    
  end;

  TCustomECBitBtn = class;

  { TECBitBtnActionLink }
  TECBitBtnActionLink = class(TWinControlActionLink)
  protected
    FClientBitBtn: TCustomECBitBtn;
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetImageIndex(Value: Integer); override;
  public
    function IsCheckedLinked: Boolean; override;
  end;

  { TCustomECBitBtn }
  TCustomECBitBtn = class(TCustomControl)
  private
    FAllowAllUp: Boolean;
    FChecked: Boolean;
    FCheckedFontColor: TColor;
    FCheckedFontStyles: TFontStyles;
    FCheckFromAction: Boolean;
    FDelay: Integer;
    FDropDownGlyph: TDropDownGlyph;
    FFlat: Boolean;
    FGlyphColor: TColor;
    FGlyphDesignChecked: TGlyphDesign;
    FGlyphDesign: TGlyphDesign;
    FGroupIndex: Integer;
    FImageIndex: TImageIndex;
    FImageIndexChecked: TImageIndex;
    FImages: TCustomImageList;
    FImageWidth: SmallInt;
    FLayout: TObjectPos;
    FMargin: SmallInt;
    FMode: TButtonMode;
    FRepeating: Integer;
    FShowCaption: Boolean;
    FSpacing: SmallInt;
    FOnChange: TNotifyEvent;
    FOnDrawGlyph: TOnDrawGlyph;
    FOnHoldDown: TNotifyEvent;
    FOnRelease: TNotifyEvent;
    FOnRepeating: TNotifyEvent;
    FTransparent: Boolean;
    procedure SetAllowAllUp(AValue: Boolean);
    procedure SetChecked(AValue: Boolean);
    procedure SetCheckedFontColor(AValue: TColor);
    procedure SetCheckedFontStyles(AValue: TFontStyles);
    procedure SetDelay(AValue: Integer);
    procedure SetDropDownGlyph(AValue: TDropDownGlyph);
    procedure SetFlat(AValue: Boolean);
    procedure SetGlyphColor(AValue: TColor);
    procedure SetGlyphDesign(AValue: TGlyphDesign);
    procedure SetGlyphDesignChecked(AValue: TGlyphDesign);
    procedure SetGroupIndex(AValue: Integer);
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetImageIndexChecked(AValue: TImageIndex);
    procedure SetImages(AValue: TCustomImageList);
    procedure SetImageWidth(AValue: SmallInt);
    procedure SetLayout(AValue: TObjectPos);
    procedure SetMargin(AValue: SmallInt);
    procedure SetMode(AValue: TButtonMode);
    procedure SetRepeating(AValue: Integer);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetSpacing(AValue: SmallInt);
    procedure SetTransparent(AValue: Boolean);
  protected const
    cDefHeight = 27;
    cDefWidth = 75;
  protected
    ActionListImagesDelay: Boolean;
    BtnDrawnPushed: Boolean;
    BtnPushed: Boolean;
    NeedRedraw: Boolean;
    PrevSize: TSize;
    RealLayout: TObjectPos;
    ECTimer: TCustomECTimer;
    ValidStates: TItemStates;
    procedure CalculatePreferredSize(var PreferredWidth, PreferredHeight: Integer;
                                     {%H-}WithThemeSpace: Boolean); override;
    procedure Click; override;
    procedure CMBiDiModeChanged(var {%H-}Message: TLMessage); message CM_BIDIMODECHANGED;
    procedure CMButtonPressed(var Message: TLMessage); message CM_BUTTONPRESSED;
    procedure CMColorChanged(var {%H-}Message: TLMessage); message CM_COLORCHANGED;
    procedure CMParentColorChanged(var Message: TLMessage); message CM_PARENTCOLORCHANGED;
    procedure CreateTimer;
    function DialogChar(var Message: TLMKey): Boolean; override;
    procedure DrawButtonBMPs;
    procedure FontChanged(Sender: TObject); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    function HasValidActImage: Boolean;
    function HasValidImages: Boolean;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Redraw;
    procedure Resize; override;
    procedure ResizeInvalidate;
    procedure SetAction(Value: TBasicAction); override;
    procedure SetAutoSize(Value: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetTimerEvent;
    procedure TextChanged; override;
    procedure TimerOnTimerDelay(Sender: TObject);
    procedure TimerOnTimerHold(Sender: TObject);
    procedure TimerOnTimerRepeating(Sender: TObject);
    procedure UpdateGroup;
    property CheckFromAction: Boolean read FCheckFromAction write FCheckFromAction;
  public
    BtnBitmaps: array [TItemState] of TBitmap;
    UpdateCount: SmallInt;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Checked: Boolean read FChecked write SetChecked default False;
    property CheckedFontColor: TColor read FCheckedFontColor write SetCheckedFontColor default clDefault;
    property CheckedFontStyles: TFontStyles read FCheckedFontStyles write SetCheckedFontStyles default cDefBtnCheckedFontStyles;
    property Delay: Integer read FDelay write SetDelay default 0;
    property DropDownGlyph: TDropDownGlyph read FDropDownGlyph write SetDropDownGlyph default edgNone;
    property Flat: Boolean read FFlat write SetFlat default False;
    property GlyphColor: TColor read FGlyphColor write SetGlyphColor default clDefault;
    property GlyphDesign: TGlyphDesign read FGlyphDesign write SetGlyphDesign;  { set default in descendants }
    property GlyphDesignChecked: TGlyphDesign read FGlyphDesignChecked write SetGlyphDesignChecked default egdNone;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property ImageIndexChecked: TImageIndex read FImageIndexChecked write SetImageIndexChecked default -1;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageWidth: SmallInt read FImageWidth write SetImageWidth default 0;
    property Layout: TObjectPos read FLayout write SetLayout default eopLeft;
    property Margin: SmallInt read FMargin write SetMargin default -1;
    property Mode: TButtonMode read FMode write SetMode default ebmButton;
    property Repeating: Integer read FRepeating write SetRepeating default 0;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property Spacing: SmallInt read FSpacing write SetSpacing default cDefBtnSpacing;
    property Transparent: Boolean read FTransparent write SetTransparent;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDrawGlyph: TOnDrawGlyph read FOnDrawGlyph write FOnDrawGlyph;
    property OnHoldDown: TNotifyEvent read FOnHoldDown write FOnHoldDown;
    property OnRelease: TNotifyEvent read FOnRelease write FOnRelease;
    property OnRepeating: TNotifyEvent read FOnRepeating write FOnRepeating;
  end;

  { TECBitBtn }
  TECBitBtn = class(TCustomECBitBtn)
  published
    property Action;
    property Align;
    property AllowAllUp;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property BorderSpacing;
    property Caption;
    property Checked;
    property CheckedFontColor;
    property CheckedFontStyles;
    {property Color;}  {does nothing ATM}
    property Constraints;
    property Delay;
    property DropDownGlyph;
    property Enabled;
    property Flat;
    property Font;
    property GlyphColor;
    property GlyphDesign default egdNone;
    property GlyphDesignChecked;
    property GroupIndex;
    property ImageIndex;
    property ImageIndexChecked;
    property Images;
    property ImageWidth;
    property Layout;
    property Margin;
    property Mode;
    property PopupMenu;
    property ParentBiDiMode;
    {property ParentColor;}  {does nothing ATM}
    property ParentFont;
    property ParentShowHint;
    property Repeating;
    property ShowCaption;
    property ShowHint;
    property Spacing;
    property TabOrder;
    property TabStop default True;
    property Visible;
    property OnChangeBounds;
    property OnChange;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawGlyph;
    property OnHoldDown;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnRelease;
    property OnRepeating;
    property OnResize;
  end;

  { TCustomECSpeedBtnPlus }
  TCustomECSpeedBtnPlus = class(TCustomECSpeedBtn)
  protected
    CustomClick: TObjectMethod;
    CustomMouseDown: TMouseMethod;
    CustomMouseUp: TMouseMethod;
    CustomResize: TObjectMethod;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override; 
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Resize; override;
  public 
    constructor Create(AOwner: TComponent); override;
  published
    property AnchorSideLeft stored False;
    property AnchorSideTop stored False;
    property AnchorSideRight stored False;
    property AnchorSideBottom stored False;     
    property Height stored False;
    property Left stored False;
    property Top stored False;
  end;
  
  { TECSpeedBtnPlus }
  TECSpeedBtnPlus = class(TCustomECSpeedBtnPlus)
  published
    property Caption;
    property Delay;
    property DropDownGlyph;
    property Flat;
    property GlyphColor;
    property GlyphDesign;
    property ImageIndex;
    property Images;
    property Layout;
    property Margin;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property Spacing;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnHoldDown;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;  
  end;
  
  { TECEditBtnSpacing }
  TECEditBtnSpacing = class(TControlBorderSpacing)
  public
    function GetSpace(Kind: TAnchorKind): Integer; override;
    procedure GetSpaceAround(var SpaceAround: TRect); override;
  end;

  { TBaseECEditBtn }
  TBaseECEditBtn = class(TCustomEdit)
  private
    FIndent: SmallInt;
    FOnVisibleChanged: TOnVisibleChanged;
    FOptions: TEBOptions;
    function GetWidthInclBtn: Integer;
    procedure SetIndent(AValue: SmallInt);
    procedure SetWidthInclBtn(AValue: Integer);
  protected
    Flags: TEditingDoneFlags;
    FAnyButton: TCustomECSpeedBtnPlus;
    function ChildClassAllowed(ChildClass: TClass): boolean; override;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    function CreateControlBorderSpacing: TControlBorderSpacing; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoOnChangeBounds; override;
    procedure InitializeWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure SetButtonPosition;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetVisible(Value: Boolean); override;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditingDone; override;
    procedure SetRealBoundRect(ARect: TRect);
    procedure SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);
    property Indent: SmallInt read FIndent write SetIndent default 0;
    property Options: TEBOptions read FOptions write FOptions default cDefEBOptions;
    property WidthInclBtn: Integer read GetWidthInclBtn write SetWidthInclBtn stored False;
    property OnVisibleChanged: TOnVisibleChanged read FOnVisibleChanged write FOnVisibleChanged;
  end;

  { TECEditBtn }
  TECEditBtn = class(TBaseECEditBtn)
  private
    FButton: TECSpeedBtnPlus;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Button: TECSpeedBtnPlus read FButton write FButton;
    property Indent;
    property Options;
    property WidthInclBtn;
  published  
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;       
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color; 
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EchoMode;
    property Enabled;
    property Font;
    property HideSelection;  
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;      
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnVisibleChanged;
  end;

const 
  cDefColorLayout = eclSystemBGR;
  cDefPrefix = '$';
    
type     
  { TECSpeedBtnColor }
  TECSpeedBtnColor = class(TCustomECSpeedBtnPlus)
  protected const
    cDefGlyphDesign = egdWinRectClr;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property GlyphDesign default cDefGlyphDesign;
    property ImageIndex;
    property Images;
    property Layout;
    property Margin;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
    property Spacing;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;               
  end;
  
  { TECColorBtn }
  TECColorBtn = class(TBaseECEditBtn)    
  private
    FButton: TECSpeedBtnColor;
    FColorLayout: TColorLayout;
    FCustomColor: TColor;
    FOnCustomColorChanged: TNotifyEvent;
    FPrefix: string;
    procedure SetColorLayout(AValue: TColorLayout);
    procedure SetCustomColor(AValue: TColor);
    procedure SetPrefix(const AValue: string);
  protected const
    cDefCustomColor = clBlack;
  protected
    FAlpha: Boolean;
    FUpdatingText: Boolean;
    procedure DoButtonClick;
    procedure RealSetText(const AValue: TCaption); override;
    procedure Redraw;
  public
    constructor Create(AOwner: TComponent); override;
    procedure EditingDone; override;
  published
    property Button: TECSpeedBtnColor read FButton write FButton;
    property ColorLayout: TColorLayout read FColorLayout write SetColorLayout default cDefColorLayout;
    property CustomColor: TColor read FCustomColor write SetCustomColor default cDefCustomColor;
    property Indent;
    property Options;
    property Prefix: string read FPrefix write SetPrefix;
    property WidthInclBtn;
    property OnCustomColorChanged: TNotifyEvent read FOnCustomColorChanged write FOnCustomColorChanged;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;       
    property BiDiMode;
    property BorderSpacing;
    property BorderStyle;  
    property CharCase;
    property Color;   
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly default True;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEditingDone;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
    property OnUTF8KeyPress;
    property OnVisibleChanged;
  end;

  { TECComboBtnSpacing }
  TECComboBtnSpacing = class(TControlBorderSpacing)
  public
    function GetSpace(Kind: TAnchorKind): Integer; override;
    procedure GetSpaceAround(var SpaceAround: TRect); override;
  end;       

const
  cComboReadOnlyStyles = [csDropDownList, csOwnerDrawFixed, csOwnerDrawVariable];

type
  { TBaseECComboBtn }
  TBaseECComboBtn = class(TCustomComboBox)
  private
    FIndent: SmallInt;
    FItemOrder: TItemOrder;
    FMaxCount: Integer;
    FOnVisibleChanged: TOnVisibleChanged;
    FOptions: TEBOptions;
    function GetWidthInclBtn: Integer;
    procedure SetIndent(AValue: SmallInt);
    procedure SetItemOrder(AValue: TItemOrder);
    procedure SetMaxCount(AValue: Integer);
    procedure SetOptions(AValue: TEBOptions);
    procedure SetWidthInclBtn(AValue: Integer);
  protected
    Flags: TEditingDoneFlags;
    FAnyButton: TCustomECSpeedBtnPlus;
    function ChildClassAllowed(ChildClass: TClass): Boolean; override;
    procedure CMBiDiModeChanged(var Message: TLMessage); message CM_BIDIMODECHANGED;
    function CreateControlBorderSpacing: TControlBorderSpacing; override;
    procedure DoExit; override;
    procedure DoOnChangeBounds; override;
    procedure InitializeWnd; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure LimitToMaxCount;
    procedure Select; override;
    procedure SetButtonPosition;
    procedure SetEnabled(Value: Boolean); override;
    procedure SetItemIndex(const Val: integer); override;
    procedure SetItems(const Value: TStrings); override;
    procedure SetParent(NewParent: TWinControl); override;
    procedure SetSorted(Val: boolean); override;
    procedure SetVisible(Value: Boolean); override;
    procedure WMKillFocus(var Message: TLMKillFocus); message LM_KILLFOCUS;
  public
    constructor Create(TheOwner: TComponent); override;
    procedure Add(const AItem: string);
    procedure AddItemHistory(const AItem: string; ACaseSensitive: Boolean);
    procedure AddItemLimit(const AItem: string; ACaseSensitive: Boolean);
    procedure EditingDone; override;
    procedure SetRealBoundRect(ARect: TRect);
    procedure SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);     
    property Indent: SmallInt read FIndent write SetIndent default 0;
    property ItemOrder: TItemOrder read FItemOrder write SetItemOrder; 
    property MaxCount: Integer read FMaxCount write SetMaxCount default 0;
    property Options: TEBOptions read FOptions write SetOptions default cDefEBOptions;
    property WidthInclBtn: Integer read GetWidthInclBtn write SetWidthInclBtn stored False;
    property OnVisibleChanged: TOnVisibleChanged read FOnVisibleChanged write FOnVisibleChanged;
  end;
  
  { TECComboBtn }      
  TECComboBtn = class(TBaseECComboBtn)
  private
    FButton: TECSpeedBtnPlus;
  protected const
    cDefItemOrder = eioFixed;
  public
    constructor Create(TheOwner: TComponent); override;
  published
    property Button: TECSpeedBtnPlus read FButton write FButton;
    property Indent;
    property ItemOrder default eioFixed;
    property Options;
    property WidthInclBtn;
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoComplete;
    property AutoCompleteText;
    property AutoDropDown;
    property AutoSelect;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemWidth;
    property MaxCount;
    property MaxLength;  
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;  
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property OnVisibleChanged;
  end;
    
  { TECColorCombo }
  TECColorCombo = class(TBaseECComboBtn)
  private
    FButton: TECSpeedBtnColor;
    FColorLayout: TColorLayout;
    FCustomColor: TColor;
    FPrefix: string;
    FOnCustomColorChanged: TNotifyEvent;
    procedure SetColorLayout(AValue: TColorLayout);
    procedure SetCustomColor(AValue: TColor);
    procedure SetPrefix(const AValue: string);
  protected const
    cDefColorOrder = eioHistory;
    cDefCustomColor = clNone;
  protected
    FAlpha: Boolean;
    FIsEnabled: Boolean;
    FLastAddedColorStr: string;
    FSelectedFromList: Boolean;
    FTextExtent: TSize;
    FUpdatingCustomColor: Boolean;
    procedure DoButtonClick;
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    procedure EnabledChanged; override;
    procedure InitializeWnd; override;
    procedure Measure;
    procedure RealSetText(const AValue: TCaption); override;
    procedure Select; override;  { only when ItemIndex changes by mouse }
    procedure SetItemIndex(const Val: Integer); override;  { only when ItemIndex changes by code }
    procedure SetItemHeight(const AValue: Integer); override;
    procedure SetItems(const Value: TStrings); override;
    procedure SetSorted(Val: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddColor(const AColorStr: string); overload;
    procedure AddColor(AColor: TColor); overload;
    procedure EditingDone; override;
    function GetColorIndex(AColor: TColor): Integer;
    procedure ResetPrefixesAndLayout(AOldLayout: TColorLayout);
    procedure SetColorText(const AColor: string);
  published           				 
    property Button: TECSpeedBtnColor read FButton write FButton;
    property ColorLayout: TColorLayout read FColorLayout write SetColorLayout default cDefColorLayout;
    property CustomColor: TColor read FCustomColor write SetCustomColor default cDefCustomColor;
    property Indent;
    property ItemOrder default cDefColorOrder;
    property MaxCount;
    property Options;
    property Prefix: string read FPrefix write SetPrefix;
    property WidthInclBtn;
    property OnCustomColorChanged: TNotifyEvent read FOnCustomColorChanged write FOnCustomColorChanged;
  published
    property Align;
    property Anchors;
    property ArrowKeysTraverseList;
    property AutoDropDown;
    property AutoSize;
    property BidiMode;
    property BorderSpacing;
    property Color;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style default csOwnerDrawFixed;
    property TabOrder;
    property TabStop;
    property Visible;  
    property OnChange;
    property OnChangeBounds;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnDropDown;
    property OnEditingDone;
    property OnEnter;
    property OnExit;
    property OnGetItems;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnSelect;
    property OnUTF8KeyPress;
    property OnVisibleChanged;
  end;
  
implementation

const cNonAlphaColors = [eclSystemBGR, eclRGBColor, eclBGRColor,
        eclCMYColor, eclYMCColor, eclHSBColor, eclBSHColor];

function DoColorBtnClick(AOwner: TComponent; AAlpha: Boolean; var AColor: TColor): Boolean;
var aAlphaChannel: Integer;
    aColorDialog: TColorDialog;
begin
  if AAlpha then
    begin
      aAlphaChannel := Integer($FF000000);
      if AColor <> clNone then aAlphaChannel := AColor and aAlphaChannel;
    end;
  aColorDialog := TColorDialog.Create(AOwner);
  aColorDialog.Color := AColor and $FFFFFF;
  Result := aColorDialog.Execute;
  if Result then
    begin
      AColor :=aColorDialog.Color;
      if AAlpha then AColor := aAlphaChannel + AColor;
    end;
  aColorDialog.Free;
end;

{ TECSpeedBtnActionLink }

procedure TECSpeedBtnActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClientSpeedBtn := TCustomECSpeedBtn(AClient);
end;

function TECSpeedBtnActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClientSpeedBtn.Checked = TCustomAction(Action).Checked);
end;

procedure TECSpeedBtnActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    begin
      FClientSpeedBtn.CheckFromAction := True;
      try
        FClientSpeedBtn.Checked := Value;
      finally
        FClientSpeedBtn.CheckFromAction := False;
      end;
    end;    
end;

procedure TECSpeedBtnActionLink.SetImageIndex(Value: Integer);
begin
  FClientSpeedBtn.Redraw;
end;

{ TCustomECSpeedBtn }

constructor TCustomECSpeedBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoFocus, csParentBackground, csReplicatable] 
                               - csMultiClicks - [csCaptureMouse, csOpaque, csSetCaption];
  FCheckedFontColor := clDefault;
  FGlyphColor := clDefault;
  FImageIndex := -1;
  FImageIndexChecked := -1;
  FLayout := eopLeft;
  RealLayout := eopLeft;
  FMargin := -1;
  FShowCaption := True;
  FSpacing := cDefBtnSpacing;
  FTransparent := True;
  PrevSize.cx := -1;
  SetInitialBounds(0, 0, cDefWidth, cDefHeight);
  ValidStates := caEnabledStates;
  AccessibleRole := larButton;
end;

procedure TCustomECSpeedBtn.Paint;
var aState: TItemState;
    bEnabled: Boolean;
begin
  {$IFDEF DBGCTRLS} DebugLn('TCustomECSpeedBtn.Paint'); {$ENDIF}
  inherited Paint;
  bEnabled := IsEnabled;
  if bEnabled xor not (eisDisabled in ValidStates) then
    begin
      if bEnabled
        then ValidStates := caEnabledStates
        else ValidStates := caDisabledStates;
      NeedRedraw := True;
    end;
  if ActionListImagesDelay then
    begin
      ActionListImagesDelay:=False;
      NeedRedraw:=True;
    end;
  if NeedRedraw then DrawButtonBMPs;
  aState := eisEnabled;
  if bEnabled then
    begin
      if BtnPushed
        then aState:= eisPushed
        else if Mode <> ebmButton then
               begin
                 if Checked or (assigned(ECTimer) and ECTimer.Enabled) then
                   begin
                     if not MouseInClient
                       then aState := eisChecked
                       else aState := eisPushedHilighted;
                   end else
                   if MouseInClient then aState := eisHighlighted;
               end else
               if MouseInClient then aState := eisHighlighted;
    end else
    begin
      if Checked 
        then aState := eisPushedDisabled
        else aState := eisDisabled;
    end;
  Canvas.Draw(0, 0, BtnBitmaps[aState]);
  BtnDrawnPushed := (aState in [eisChecked, eisPushed, eisPushedHilighted]);
end;

{$MACRO ON}
{$DEFINE BTNCLASSNAME:=TCustomECSpeedBtn}
{$include ecxbtn.inc}

{ TECBitBtnActionLink }

procedure TECBitBtnActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClientBitBtn := TCustomECBitBtn(AClient);
end;

function TECBitBtnActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked and (FClientBitBtn.Checked = TCustomAction(Action).Checked);
end;

procedure TECBitBtnActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then
    begin
      FClientBitBtn.CheckFromAction := True;
      try
        FClientBitBtn.Checked := Value;
      finally
        FClientBitBtn.CheckFromAction := False;
      end;
    end;
end;

procedure TECBitBtnActionLink.SetImageIndex(Value: Integer);
begin
  FClientBitBtn.Redraw;
end;

{ TCustomECBitBtn }

constructor TCustomECBitBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csParentBackground, csReplicatable, csSetCaption]
                               - csMultiClicks - [csCaptureMouse, csNoFocus, csOpaque];
  FCheckedFontColor := clDefault;
  FGlyphColor := clDefault;
  FImageIndex := -1;
  FImageIndexChecked := -1;
  FLayout := eopLeft;
  RealLayout := eopLeft;
  FMargin := -1;
  FShowCaption := True;
  FSpacing := cDefBtnSpacing;
  FTransparent := True;
  PrevSize.cx := -1;
  SetInitialBounds(0, 0, cDefWidth, cDefHeight);
  TabStop := True;
  ValidStates := caEnabledStates;
  AccessibleRole := larButton;
end;

procedure TCustomECBitBtn.Paint;
var aState: TItemState;
    bEnabled: Boolean;
begin
  {$IFDEF DBGCTRLS} DebugLn('TCustomECBitBtn.Paint'); {$ENDIF}
  inherited Paint;
  bEnabled := IsEnabled;
  if bEnabled xor not (eisDisabled in ValidStates) then
    begin
      if bEnabled
        then ValidStates := caEnabledStates
        else ValidStates := caDisabledStates;
      NeedRedraw := True;
    end;
  if ActionListImagesDelay then
    begin
      ActionListImagesDelay:=False;
      NeedRedraw:=True;
    end;
  if NeedRedraw then DrawButtonBMPs;
  aState := eisEnabled;
  if bEnabled then
    begin
      if BtnPushed
        then aState:= eisPushed
        else if Mode <> ebmButton then
               begin
                 if Checked or (assigned(ECTimer) and ECTimer.Enabled) then
                   begin
                     if not MouseInClient
                       then aState := eisChecked
                       else aState := eisPushedHilighted;
                   end else
                   if MouseInClient then aState := eisHighlighted;
               end else
               if MouseInClient then aState := eisHighlighted;
    end else
    begin
      if Checked
        then aState := eisPushedDisabled
        else aState := eisDisabled;
    end;
  Canvas.Draw(0, 0, BtnBitmaps[aState]);
  if Focused then Canvas.DrawFocusRectNonThemed(Rect(3, 3, Width - 3, Height - 3));
  BtnDrawnPushed := (aState in [eisChecked, eisPushed, eisPushedHilighted]);
end;

{$DEFINE BTNCLASSNAME:=TCustomECBitBtn}
{$include ecxbtn.inc}

{ TCustomECSpeedBtnPlus }

constructor TCustomECSpeedBtnPlus.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csNoDesignSelectable];
  SetSubComponent(True);
end;

procedure TCustomECSpeedBtnPlus.Click;
begin
  inherited Click;
  if assigned(CustomClick) then CustomClick;
end;     

procedure TCustomECSpeedBtnPlus.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if assigned(CustomMouseDown) then CustomMouseDown(Button, Shift);
  if assigned(Owner) and (Owner is TWinControl) and TWinControl(Owner).CanFocus
    then TWinControl(Owner).SetFocus;
end;    

procedure TCustomECSpeedBtnPlus.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if assigned(CustomMouseUp) then CustomMouseUp(Button, Shift);
end;    

procedure TCustomECSpeedBtnPlus.Resize;
begin
  inherited Resize;
  if assigned(CustomResize) then CustomResize;
end;    

{ TECEditBtnSpacing }

function TECEditBtnSpacing.GetSpace(Kind: TAnchorKind): Integer;
begin
  Result:=inherited GetSpace(Kind);
  case Kind of
    akLeft: if Control.IsRightToLeft then
              inc(Result, TBaseECEditBtn(Control).FAnyButton.Width + TBaseECEditBtn(Control).Indent);
    akRight: if not Control.IsRightToLeft then
               inc(Result, TBaseECEditBtn(Control).FAnyButton.Width + TBaseECEditBtn(Control).Indent);
  end;              
end;

procedure TECEditBtnSpacing.GetSpaceAround(var SpaceAround: TRect);
var aIndentButtonWidth: Integer;
begin
  inherited GetSpaceAround(SpaceAround);
  with TBaseECEditBtn(Control) do
    aIndentButtonWidth := FAnyButton.Width + Indent;
  if not Control.IsRightToLeft 
    then inc(SpaceAround.Right, aIndentButtonWidth)
    else inc(SpaceAround.Left, aIndentButtonWidth);
end;
    
{ TBaseECEditBtn }

constructor TBaseECEditBtn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  with FAnyButton do
    begin
      AnchorParallel(akTop, 0, self);
      AnchorParallel(akBottom, 0, self);   
      CustomResize := @SetButtonPosition;
    end;
  FOptions := cDefEBOptions;
  AccessibleRole := larTextEditorSingleline;
end;

function TBaseECEditBtn.ChildClassAllowed(ChildClass: TClass): boolean;
begin
  Result := (ChildClass = TCustomECSpeedBtnPlus);
end;

procedure TBaseECEditBtn.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  SetButtonPosition;
end;

function TBaseECEditBtn.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TECEditBtnSpacing.Create(self);
end;

procedure TBaseECEditBtn.DoEnter;
begin
  inherited DoEnter;
  if (eboInCellEditor in Options) and FAnyButton.MouseInClient then CaretPos:=Point(length(Text), 1);
end;

procedure TBaseECEditBtn.DoExit;
begin
  if eboInCellEditor in Options then
    if not (edfAllowDoExitInCell in Flags)
      then exit  { Exit! }
      else exclude(Flags, edfAllowDoExitInCell);
  inherited DoExit;
end;

procedure TBaseECEditBtn.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  SetButtonPosition;
end;

procedure TBaseECEditBtn.EditingDone;
begin
  if (edfForceEditingDone in Flags) or not FAnyButton.MouseInClient then
    begin
      if edfForceEditingDone in Flags then include(Flags, edfAllowDoExitInCell);
      exclude(Flags, edfForceEditingDone);
      inherited EditingDone;
    end;
end;

procedure TBaseECEditBtn.InitializeWnd;
begin
  inherited InitializeWnd;
  SetButtonPosition;
end; 
      
procedure TBaseECEditBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if eboInCellEditor in Options then include(Flags, edfForceEditingDone);
  case Key of
    VK_RETURN: 
      if ((ssModifier in Shift) and (eboClickCtrlEnter in FOptions)) or 
        ((ssAlt in Shift) and (eboClickAltEnter in FOptions)) or
        ((ssShift in Shift) and (eboClickShiftEnter in FOptions)) 
        then FAnyButton.Click
        else if eboInCellEditor in Options then Flags := Flags + [edfEnterWasInKeyDown, edfForceEditingDone];
    VK_SPACE: 
      if (ssModifier in Shift) or ReadOnly then FAnyButton.Click;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TBaseECEditBtn.KeyUp(var Key: Word; Shift: TShiftState);
var b: Boolean;
begin
  if (eboInCellEditor in Options) and not (edfEnterWasInKeyDown in Flags) then Key := 0;
  b := (Key = VK_RETURN);
  if b then
    begin
      b := not (((ssModifier in Shift) and (eboClickCtrlEnter in Options)) or
                ((ssAlt in Shift) and (eboClickAltEnter in Options)) or
                ((ssShift in Shift) and (eboClickShiftEnter in Options)));
      if not b then Key := 0;
    end;
  if b and not (eboInCellEditor in Options)
    then include(Flags, edfForceEditingDone)
    else exclude(Flags, edfForceEditingDone);
  inherited KeyUp(Key, Shift);
end;

procedure TBaseECEditBtn.SetButtonPosition;
begin
  if not IsRightToLeft
    then FAnyButton.Left := Left + Width + Indent
    else if Left >= 0 then FAnyButton.Left := Left - Indent - FAnyButton.Width;
end;    

procedure TBaseECEditBtn.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value);        
  FAnyButton.Enabled := Value;
end;    

procedure TBaseECEditBtn.SetParent(NewParent: TWinControl);
begin
  FAnyButton.Anchors := [];
  inherited SetParent(NewParent);
  FAnyButton.Parent := Parent;
  FAnyButton.Anchors := [akTop, akBottom];
end;

procedure TBaseECEditBtn.SetRealBoundRect(ARect: TRect);
begin
  if BiDiMode = bdLeftToRight 
    then dec(ARect.Right, Indent + FAnyButton.Width)
    else inc(ARect.Left, Indent + FAnyButton.Width);
  BoundsRect := ARect;
end; 
          
procedure TBaseECEditBtn.SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if BiDiMode <> bdLeftToRight then ALeft := ALeft + Indent + FAnyButton.Width;
  SetBounds(ALeft, ATop, AWidth - Indent - FAnyButton.Width, AHeight);
end;                  

procedure TBaseECEditBtn.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FAnyButton.Visible := Value;
  if Value and (eboInCellEditor in Options) then exclude(Flags, edfEnterWasInKeyDown);
  if assigned(OnVisibleChanged) then OnVisibleChanged(self, Value);
end;

procedure TBaseECEditBtn.WMKillFocus(var Message: TLMKillFocus);
begin
  if eboInCellEditor in Options then
    if not (edfForceEditingDone in Flags) and FAnyButton.MouseInClient then exit;  { Exit! }
  include(Flags, edfForceEditingDone);
  inherited WMKillFocus(Message);
end;

{ Setters }

function TBaseECEditBtn.GetWidthInclBtn: Integer;
begin
  Result := Width + Indent + FAnyButton.Width;
end;     

procedure TBaseECEditBtn.SetIndent(AValue: SmallInt);
begin
  if FIndent = AValue then exit;
  FIndent := AValue;
  SetButtonPosition;
end;

procedure TBaseECEditBtn.SetWidthInclBtn(AValue: Integer);
begin
  Width := AValue - Indent - FAnyButton.Width;
end;

{ TECEditBtn }

constructor TECEditBtn.Create(AOwner: TComponent);
begin
  FButton := TECSpeedBtnPlus.Create(self);
  FAnyButton := FButton;
  FButton.Name := 'ECEditBtnButton';
  inherited Create(AOwner);    
end;   

{ TECSpeedBtnColor }

constructor TECSpeedBtnColor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FGlyphDesign := cDefGlyphDesign;
end;        

{ TECColorBtn }

constructor TECColorBtn.Create(AOwner: TComponent);
begin
  FButton := TECSpeedBtnColor.Create(self);
  FAnyButton := FButton;
  with FButton do
    begin  
      CustomClick := @DoButtonClick;
      Name := 'ECCBSpeedBtn';
      Width := 27;
    end;     
  inherited Create(AOwner);
  ReadOnly := True;
  FCustomColor := cDefCustomColor;
  FPrefix := cDefPrefix;
  Redraw;
end;

procedure TECColorBtn.DoButtonClick;
var aCustomColor: TColor;
begin
  aCustomColor := CustomColor;
  if DoColorBtnClick(self, FAlpha, aCustomColor) then CustomColor := aCustomColor;
  SetFocus;
end;        

procedure TECColorBtn.EditingDone;
var aColor: TColor;
begin
  if not ReadOnly then
    if TryStrToColorLayouted(Text, ColorLayout, aColor) then CustomColor := aColor;
  inherited EditingDone;
end;

procedure TECColorBtn.RealSetText(const AValue: TCaption);
var aColor: TColor;
    b: Boolean;
begin
  b := False;
  if not FUpdatingText then
    if TryStrToColorLayouted(AValue, ColorLayout, aColor) then
      begin
        b := (aColor <> FCustomColor);
        if b then
          begin
            FCustomColor := aColor;
            FButton.GlyphColor := aColor;
          end;
      end else
      exit;  { Exit! }
  inherited RealSetText(AValue);
  if b and assigned(OnCustomColorChanged) then OnCustomColorChanged(self);
end;

procedure TECColorBtn.Redraw;
var aColor: string;
begin
  aColor := ColorToStrLayouted(CustomColor, ColorLayout, Prefix);
  FUpdatingText := True;
  Text := aColor;
  FUpdatingText := False;
end;

{ Setters }

procedure TECColorBtn.SetCustomColor(AValue: TColor);
begin
  if FCustomColor = AValue then exit;
  FCustomColor := AValue;
  FButton.GlyphColor := AValue and $FFFFFF;
  Redraw;
  if assigned(OnCustomColorChanged) then OnCustomColorChanged(self);
end;

procedure TECColorBtn.SetColorLayout(AValue: TColorLayout);
begin
  if FColorLayout = AValue then exit;
  FColorLayout := AValue;
  FAlpha := (AValue in cNonAlphaColors);
  Redraw;
end;

procedure TECColorBtn.SetPrefix(const AValue: string);
begin
  if FPrefix = AValue then exit;
  FPrefix := AValue;
  Redraw;
end;

{ TECComboBtnSpacing }

function TECComboBtnSpacing.GetSpace(Kind: TAnchorKind): Integer;
begin
  Result:=inherited GetSpace(Kind);
  case Kind of
    akLeft: if Control.IsRightToLeft then
              inc(Result, TBaseECComboBtn(Control).FAnyButton.Width + TBaseECComboBtn(Control).Indent);
    akRight: if not Control.IsRightToLeft then
               inc(Result, TBaseECComboBtn(Control).FAnyButton.Width + TBaseECComboBtn(Control).Indent);
  end;  
end;

procedure TECComboBtnSpacing.GetSpaceAround(var SpaceAround: TRect);
var aIndentButtonWidth: Integer;
begin
  inherited GetSpaceAround(SpaceAround);
  with TBaseECComboBtn(Control) do
    aIndentButtonWidth := FAnyButton.Width + Indent;
  if not Control.IsRightToLeft 
    then inc(SpaceAround.Right, aIndentButtonWidth)
    else inc(SpaceAround.Left, aIndentButtonWidth);
end;   

{ TBaseECComboBtn }

constructor TBaseECComboBtn.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  ControlStyle := ControlStyle - [csSetCaption];
  with FAnyButton do
    begin
      AnchorParallel(akTop, 0, self);
      AnchorParallel(akBottom, 0, self);
      CustomResize := @SetButtonPosition;
    end;
  FOptions := cDefEBOptions;
  TStringList(Items).Duplicates := dupIgnore;
  AccessibleRole := larComboBox;
end;

procedure TBaseECComboBtn.Add(const AItem: string);
begin
  case ItemOrder of
    eioFixed: AddItemLimit(AItem, False);
    eioHistory: AddItemHistory(AItem, False);
    eioSorted: AddItemLimit(AItem, False);
  end;
end;

procedure TBaseECComboBtn.AddItemHistory(const AItem: string; ACaseSensitive: Boolean);
var aMaxCount: Integer;
begin
  aMaxCount := MaxCount;
  if aMaxCount <= 0 then aMaxCount := high(Integer);
  AddHistoryItem(aItem, aMaxCount, True, ACaseSensitive);
end;

procedure TBaseECComboBtn.AddItemLimit(const AItem: string; ACaseSensitive: Boolean);
var i, aCount: Integer;
begin
  if TStringList(Items).Duplicates <> dupAccept then
    begin
      if not ACaseSensitive then
        begin
          for i := Items.Count -1 downto 0 do
            if AnsiCompareText(Items[i], AItem) = 0 then exit;  { Exit! }
        end else
        begin
          for i := Items.Count -1 downto 0 do
            if Items[i] = AItem then exit;  { Exit! }
        end;
    end;
  Items.BeginUpdate;
  aCount := MaxCount - 1;  { remove overflow item(s)+1 from the beginning }
  if aCount >= 0 then      { and it works on sorted list too, so beware }
    for i := 1 to Items.Count - aCount do
      Items.Delete(0);
  Items.Add(AItem);  { add new item to the end; Insert not allowed on sorted list }
  Items.EndUpdate;
end;

function TBaseECComboBtn.ChildClassAllowed(ChildClass: TClass): Boolean;
begin
  Result := (ChildClass = TCustomECSpeedBtnPlus);
end;

procedure TBaseECComboBtn.CMBiDiModeChanged(var Message: TLMessage);
begin
  inherited CMBiDiModeChanged(Message);
  SetButtonPosition;
end;  

function TBaseECComboBtn.CreateControlBorderSpacing: TControlBorderSpacing;
begin
  Result := TECComboBtnSpacing.Create(self);
end;

procedure TBaseECComboBtn.DoExit;
begin
  if eboInCellEditor in Options then
    if not (edfAllowDoExitInCell in Flags)
      then exit  { Exit! }
      else exclude(Flags, edfAllowDoExitInCell);
  inherited DoExit;
end;

procedure TBaseECComboBtn.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  SetButtonPosition;
end;

procedure TBaseECComboBtn.EditingDone;
begin
  if (edfForceEditingDone in Flags) or not FAnyButton.MouseInClient then
    begin
      if edfForceEditingDone in Flags then include(Flags, edfAllowDoExitInCell);
      exclude(Flags, edfForceEditingDone);
      inherited EditingDone;
    end;
end;

procedure TBaseECComboBtn.InitializeWnd;
begin
  inherited InitializeWnd;
  SetButtonPosition;
end;

procedure TBaseECComboBtn.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if eboInCellEditor in Options then include(Flags, edfForceEditingDone);
  case Key of 
    VK_RETURN:
      begin
        if ((ssModifier in Shift) and (eboClickCtrlEnter in Options)) or
          ((ssAlt in Shift) and (eboClickAltEnter in Options)) or
          ((ssShift in Shift) and (eboClickShiftEnter in Options)) 
          then FAnyButton.Click
          else if eboInCellEditor in Options then Flags := Flags + [edfEnterWasInKeyDown, edfForceEditingDone];
      end;
    VK_SPACE: if (ssModifier in Shift) or (Style in cComboReadOnlyStyles) then FAnyButton.Click;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TBaseECComboBtn.KeyUp(var Key: Word; Shift: TShiftState);
var b: Boolean;
begin
  if (eboInCellEditor in Options) and not (edfEnterWasInKeyDown in Flags) then Key := 0;
  b := (Key = VK_RETURN);
  if b then
    begin
      b := not (((ssModifier in Shift) and (eboClickCtrlEnter in Options)) or
                ((ssAlt in Shift) and (eboClickAltEnter in Options)) or
                ((ssShift in Shift) and (eboClickShiftEnter in Options)));
      if not b then Key := 0;
    end;
  if b and not (eboInCellEditor in Options)
    then include(Flags, edfForceEditingDone)
    else exclude(Flags, edfForceEditingDone);
  inherited KeyUp(Key, Shift);
end;

procedure TBaseECComboBtn.LimitToMaxCount;
var i, aMaxCount: Integer;
begin
  aMaxCount := MaxCount;
  if (aMaxCount > 0) and (aMaxCount < Items.Count) then
    begin
      Items.BeginUpdate;
      if ItemOrder = eioFixed
        then
          for i := 0 to Items.Count - aMaxCount - 1 do
            Items.Delete(i)
        else
          for i := Items.Count - 1 downto aMaxCount do
            Items.Delete(i);
      Items.EndUpdate;
    end;
end;

procedure TBaseECComboBtn.Select;
begin
  if (ItemIndex > 0) and (ItemOrder = eioHistory) then
    begin
      Items.Move(ItemIndex, 0);
      ItemIndex := 0;
    end;
  inherited Select;
end;

procedure TBaseECComboBtn.SetButtonPosition;
begin
  if not IsRightToLeft
    then FAnyButton.Left := Left + Width + Indent
    else if Left >= 0 then FAnyButton.Left := Left - Indent - FAnyButton.Width;
end;

procedure TBaseECComboBtn.SetEnabled(Value: Boolean);
begin
  inherited SetEnabled(Value); 
  FAnyButton.Enabled := Value;
end;

procedure TBaseECComboBtn.SetParent(NewParent: TWinControl);
begin
  FAnyButton.Anchors := [];
  inherited SetParent(NewParent);
  FAnyButton.Parent := NewParent;
  FAnyButton.Anchors := [akTop, akBottom];
end;        

procedure TBaseECComboBtn.SetRealBoundRect(ARect: TRect);
begin
  if BiDiMode = bdLeftToRight 
    then dec(ARect.Right, Indent + FAnyButton.Width)
    else inc(ARect.Left, Indent + FAnyButton.Width);
  BoundsRect := ARect;    
end;

procedure TBaseECComboBtn.SetRealBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if BiDiMode <> bdLeftToRight then ALeft := ALeft + Indent + FAnyButton.Width;
  SetBounds(ALeft, ATop, AWidth - Indent - FAnyButton.Width, AHeight);  
end;        

procedure TBaseECComboBtn.SetSorted(Val: boolean);
begin
  if Val then FItemOrder := eioSorted;
  inherited SetSorted(Val);
end;         

procedure TBaseECComboBtn.SetVisible(Value: Boolean);
begin
  inherited SetVisible(Value);
  FAnyButton.Visible := Value;
  if Value and (eboInCellEditor in Options) then exclude(Flags, edfEnterWasInKeyDown);
  if assigned(OnVisibleChanged) then OnVisibleChanged(self, Value);
end;

procedure TBaseECComboBtn.WMKillFocus(var Message: TLMKillFocus);
begin
  if eboInCellEditor in Options then
    if not (edfForceEditingDone in Flags) and FAnyButton.MouseInClient then exit;  { Exit! }
  include(Flags, edfForceEditingDone);
  inherited WMKillFocus(Message);
end;

{ TBaseECComboBtn.Setters }

function TBaseECComboBtn.GetWidthInclBtn: Integer;
begin
  Result := Width + Indent + FAnyButton.Width;
end;       

procedure TBaseECComboBtn.SetIndent(AValue: SmallInt);
begin
  if FIndent = AValue then exit;
  FIndent := AValue;
  SetButtonPosition;
end;

procedure TBaseECComboBtn.SetItemIndex(const Val: integer);
var aValue: Integer;
begin
  aValue := Val;
  if not (csLoading in ComponentState) and (ItemOrder = eioHistory) then
    begin
      if (aValue > 0) and (aValue < Items.Count) then
        begin
          Items.Move(aValue, 0);
          aValue := 0;
        end;
    end;
  inherited SetItemIndex(aValue);
end;

procedure TBaseECComboBtn.SetItemOrder(AValue: TItemOrder);
begin
  if FItemOrder = AValue then exit;
  FItemOrder := AValue;
  Sorted := (AValue = eioSorted);
end;

procedure TBaseECComboBtn.SetItems(const Value: TStrings);
begin
  inherited SetItems(Value);
  LimitToMaxCount;
end;

procedure TBaseECComboBtn.SetMaxCount(AValue: Integer);
begin
  if AValue < 0 then AValue := 0;
  if FMaxCount = AValue then exit;
  FMaxCount := AValue;
  LimitToMaxCount;
end;     

procedure TBaseECComboBtn.SetOptions(AValue: TEBOptions);
begin
  if FOptions = AValue then exit;
  FOptions := AValue;
end;

procedure TBaseECComboBtn.SetWidthInclBtn(AValue: Integer);
begin
  Width := AValue - Indent - FAnyButton.Width;
end;

{ TECComboBtn }

constructor TECComboBtn.Create(TheOwner: TComponent);
begin
  FButton := TECSpeedBtnPlus.Create(self);
  FAnyButton := FButton;
  FButton.Name := 'ECCSpeedBtn';
  FItemOrder := cDefItemOrder;
  inherited Create(TheOwner); 
end;

{ TECColorCombo }

constructor TECColorCombo.Create(AOwner: TComponent);
begin
  FButton := TECSpeedBtnColor.Create(self);
  FAnyButton := FButton;
  with FButton do
    begin  
      CustomClick := @DoButtonClick;
      GlyphColor := clBtnFace;  { ~clNone }
      Name := 'ECCCSpeedBtn';
      Width := 27;
    end;
  FLastAddedColorStr := 'clNone';
  inherited Create(AOwner);
  FCustomColor := cDefCustomColor;
  FItemOrder := cDefColorOrder;
  FIsEnabled := True;
  FPrefix := cDefPrefix;
  Style := csOwnerDrawFixed;
end;         

procedure TECColorCombo.AddColor(const AColorStr: string);
var aColor: TColor;
begin
  if TryStrToColorLayouted(AColorStr, ColorLayout, aColor) then AddColor(aColor);
end;        

procedure TECColorCombo.AddColor(AColor: TColor);
var aColorStr: string;
begin
  aColorStr := ColorToStrLayouted(AColor, ColorLayout, Prefix);
  if not (Style in cComboReadOnlyStyles) then FLastAddedColorStr := aColorStr;
  case ItemOrder of
    eioFixed:
      begin
        AddItemLimit(aColorStr, False);
        ItemIndex := Items.Count - 1;
      end;
    eioHistory:
      begin
        AddItemHistory(aColorStr, False);
        ItemIndex := 0;  
      end;
    eioSorted:
      begin
        AddItemLimit(aColorStr, False);
        ItemIndex := Items.IndexOf(aColorStr);
      end;
  end;
end;

procedure TECColorCombo.DoButtonClick;
var aCustomColor: TColor;
begin
  aCustomColor := CustomColor;
  if DoColorBtnClick(self, FAlpha, aCustomColor) then CustomColor := aCustomColor;
  SetFocus;
end;         

procedure TECColorCombo.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
var aColor: TColor;
    bFocusedEditableMainItemNoDD: Boolean;  { combo has edit-like line edit in csDropDownList (Win) and is closed (not DroppedDown }
begin  { do not call inherited ! }
  {$IFDEF DBGCTRLS} DebugLn('DrawItem ', ColorToString(Canvas.Brush.Color)); {$ENDIF}
  {$IF DEFINED(LCLWin32) OR DEFINED(LCLWin64)}
  bFocusedEditableMainItemNoDD := (Focused and (ARect.Left > 0) and not DroppedDown);
  {$ELSE}
  bFocusedEditableMainItemNoDD := False;
  {$ENDIF}
  if not (odSelected in State) then Canvas.Brush.Color := clWindow;
  if (ARect.Left = 0) or bFocusedEditableMainItemNoDD then Canvas.FillRect(ARect);
  Canvas.Brush.Style := bsClear;
  if (not (odSelected in State) or (ARect.Left > 0)) and not bFocusedEditableMainItemNoDD
    then Canvas.Font.Color := GetColorResolvingDefault(Font.Color, clWindowText)
    else Canvas.Font.Color := clHighlightText;
  if bFocusedEditableMainItemNoDD then
    begin
      LCLIntf.SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
      LCLIntf.DrawFocusRect(Canvas.Handle, aRect);
    end;
  inc(ARect.Left, 3);
  Canvas.TextOut(ARect.Left, (ARect.Top + ARect.Bottom - FTextExtent.cy) div 2, Items[Index]);
  if TryStrToColorLayouted(Items[Index], ColorLayout, aColor) then 
    with Canvas do
      begin
        Pen.Color := GetColorResolvingEnabled(clWindowText, FIsEnabled);
        Brush.Color := GetColorResolvingEnabled(aColor, FIsEnabled);
        Brush.Style := bsSolid;
        Rectangle(ARect.Left + FTextExtent.cx + 2, ARect.Top + 1,
                  ARect.Right - 3, ARect.Bottom - 1);
      end;
end;

procedure TECColorCombo.EditingDone;
begin
  inherited EditingDone;
  if not (Style in cComboReadOnlyStyles) then SetColorText(Text);
end;

procedure TECColorCombo.EnabledChanged;
begin
  inherited EnabledChanged;
  FIsEnabled := IsEnabled;
end;

function TECColorCombo.GetColorIndex(AColor: TColor): Integer;
var i: Integer;
    aColorDD: TColor;
begin
  Result := -1;
  AColor := ColorToRGB(AColor);
  for i := 0 to Items.Count-1 do
    begin
      if TryStrToColorLayouted(Items[i], ColorLayout, aColorDD) then
        if ColorToRGB(aColorDD) = AColor then
          begin
            Result := i;
            break;
          end;
    end;
end;

procedure TECColorCombo.InitializeWnd;
begin
  inherited InitializeWnd;
  Measure;
end;

procedure TECColorCombo.Measure;
begin
  if HandleAllocated then
    case ColorLayout of
      eclSystemBGR: FTextExtent := Canvas.TextExtent('clMoneyGreen');
      eclRGBColor, eclBGRColor, eclCMYColor, eclYMCColor, eclHSBColor, eclBSHColor:
        FTextExtent := Canvas.TextExtent(Prefix + 'F9CDEB')
      otherwise FTextExtent := Canvas.TextExtent(Prefix + 'F9CDEBA8');
    end;
end;

procedure TECColorCombo.RealSetText(const AValue: TCaption);
begin
  SetColorText(AValue);
  if not (Style in cComboReadOnlyStyles) then inherited RealSetText(FLastAddedColorStr);
end;

procedure TECColorCombo.ResetPrefixesAndLayout(AOldLayout: TColorLayout);
var i: Integer;
    aColor: TColor;
begin
  for i := 0 to Items.Count - 1 do
    if TryStrToColorLayouted(Items[i], AOldLayout, aColor) then
      Items[i] := ColorToStrLayouted(aColor, ColorLayout, Prefix);
end;

procedure TECColorCombo.Select;  { only when ItemIndex changes by mouse }
var aColor: TColor;
begin
  inherited Select;
  if TryStrToColorLayouted(Items[ItemIndex], ColorLayout, aColor) then
    begin
      FSelectedFromList := True;
      CustomColor := aColor;
      FSelectedFromList := False;
    end;
end;

procedure TECColorCombo.SetColorText(const AColor: string);
var aCustomColor: TColor;
begin
  if TryStrToColorLayouted(AColor, ColorLayout, aCustomColor) then CustomColor := aCustomColor;
end;

procedure TECColorCombo.SetItemIndex(const Val: Integer);  { only when ItemIndex changes by code }
var aColor: TColor;
    aValue: Integer;
begin
  {$IFDEF DBGCTRLS} DebugLn('TECColorCombo.SetItemIndex ' + intToStr(Val)); {$ENDIF}
  inherited SetItemIndex(Val);
  aValue := ItemIndex;  { Val may be changed in inherited }
  if not (csLoading in ComponentState) then
    begin
      if aValue >= 0 then
        begin
          if TryStrToColorLayouted(Items[aValue] , ColorLayout, aColor) then
            if not FUpdatingCustomColor
              then CustomColor := aColor
              else FCustomColor := aColor;
        end else
        CustomColor := clNone;
    end;
end;

procedure TECColorCombo.SetItems(const Value: TStrings);
var aColor: TColor;
begin
  aColor := CustomColor;
  inherited SetItems(Value);
  if Value.Count > 0
    then ItemIndex := GetColorIndex(aColor)
    else CustomColor := clNone;
end;

procedure TECColorCombo.SetSorted(Val: Boolean);
var aColor: TColor;
begin
  inherited SetSorted(Val);
  if TryStrToColorLayouted(Text, ColorLayout, aColor) then CustomColor := aColor;
end;

{ TECColorCombo.Setters }

procedure TECColorCombo.SetColorLayout(AValue: TColorLayout);
var aOldLayout: TColorLayout;
begin                        
  if FColorLayout = AValue then exit;
  aOldLayout := FColorLayout;
  FColorLayout := AValue;
  FAlpha := (AValue in cNonAlphaColors);
  Measure;
  ResetPrefixesAndLayout(aOldLayout);
end;

procedure TECColorCombo.SetCustomColor(AValue: TColor);
begin
  {$IFDEF DBGCTRLS} DebugLn('SetCustomColor ', ColorToString(AValue)); {$ENDIF}
  if FCustomColor = AValue then exit;
  FCustomColor := AValue;
  if AValue = clNone then
    begin
      FButton.GlyphColor := clBtnFace;
      ItemIndex := -1;
    end else
    FButton.GlyphColor := AValue and $FFFFFF;
  if not (csLoading in ComponentState) then
    begin
      FUpdatingCustomColor := True;
      if not FSelectedFromList and (AValue <> clNone) then AddColor(AValue);
      if assigned(OnCustomColorChanged) then OnCustomColorChanged(self);
      FUpdatingCustomColor := False;
    end;
end;

procedure TECColorCombo.SetItemHeight(const AValue: Integer);
begin
  inherited SetItemHeight(AValue);
  Measure;
end;

procedure TECColorCombo.SetPrefix(const AValue: string);
begin
  if FPrefix = AValue then exit;
  FPrefix := AValue;                          
  Measure;
  ResetPrefixesAndLayout(ColorLayout);
end;

end.


