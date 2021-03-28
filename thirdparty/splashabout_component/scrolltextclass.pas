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

 * This unit is part of the visual component SplashAbout
 * Code is adapted from the Lazarus IDE code
 * by minesadorada@charcodelvalle.com
 }
unit scrolltextclass;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,LCLIntf;

CONST
  C_TEXTFILENAME='credits.txt';
  C_VERSION='1.0.0.0';

type
  TScrollTextClass = class(TGraphicControl)
  private
    FActive: boolean;
    FActiveLine: integer;   //the line over which the mouse hovers
    FBuffer: TBitmap;
    FEndLine: integer;
    FLineHeight: integer;
    FLines: TStrings;
    FNumLines: integer;
    FOffset: integer;
    FStartLine: integer;
    FStepSize: integer;
    FTimer: TTimer;
    FFont:TFont;
    FBackColor:TColor;
    FUseTextFile:Boolean;
    fTextFileName:String;
    fVersionString:String;
    function ActiveLineIsURL: boolean;
    procedure DoTimer(Sender: TObject);
    procedure SetActive(const AValue: boolean);
    procedure Init;
    procedure DrawScrollingText(Sender: TObject);
    Procedure SetLines(AValue:TStrings);
    Procedure SetFont(AValue:TFont);
  protected
    procedure DoOnChangeBounds; override;
    procedure MouseDown(Button: TMouseButton; Shift:TShiftState; X,Y:Integer); override;
    procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Can be set in design mode. Note URL links are inactive in design mode
    property Active: boolean read FActive write SetActive;
    // Inherited property
    property Align;
    // Inherited property
    property Borderspacing;
    // Can be set in design or runtime mode. UseTextFile property overrides.
    property Lines: TStrings read FLines write SetLines;
    // Sets the background color of the window
    property BackColor:TColor read fBackColor write fBackColor default clWindow;
    // Sets the font properties of the scrolling text
    property Font:TFont read fFont Write SetFont;
    // If TRUE then the file 'scrolling.txt' sould be in the application folder
    property UseTextFile:Boolean read fUseTextFile write fUseTextFile default FALSE;
    // Read-only property to remind you of the correct file name
    property TextFileName:String read fTextFileName;
    // Version number of this component
    property Version:String read fVersionString;
  end;

implementation
Procedure TScrollTextClass.SetFont(AValue:TFont);
begin
  fFont.Assign(AValue);
end;

Procedure TScrollTextClass.SetLines(AValue:TStrings);
begin
  fLines.Assign(AValue);
end;

procedure TScrollTextClass.SetActive(const AValue: boolean);
begin
  FActive := AValue;
  if FActive then
    Init;
  FTimer.Enabled:=Active;
end;

procedure TScrollTextClass.Init;
begin
  FBuffer.Width := Width;
  FBuffer.Height := Height;
  FLineHeight := FBuffer.Canvas.TextHeight('X');
  FNumLines := FBuffer.Height div FLineHeight;

  if FOffset = -1 then
    FOffset := FBuffer.Height;

  with FBuffer.Canvas do
  begin
    Brush.Color := fBackColor;
    Brush.Style := bsSolid;
    FillRect(0, 0, Width, Height);
  end;
   If (fLines.Count = 0) then
     begin
         fLines.Add('This is the Credits scrolling window.');
         fLines.Add(' ');
         fLines.Add('This default text is showing because you either:');
         fLines.Add(' ');
         fLines.Add('1) Haven''t set any text in the CreditLines property. or');
         fLines.Add('2) the file specified in the ');
         fLines.Add('CreditsTextFileName property is absent or empty.');
         fLines.Add(' ');
         fLines.Add('Note that URL links such as');
         fLines.Add('http://http://wiki.lazarus.freepascal.org/Main_Page');
         fLines.Add('are clickable by the user');
         fLines.Add(' ');
         fLines.Add(' ');
         fLines.Add('The standalone visual component TScrollTextClass is available at:');
         fLines.Add('http://www.charcodelvalle.com/scrollingtext/scrollingtext_component.zip');
         fLines.Add(' ');
         fLines.Add('June 2014');
     end;
end;

procedure TScrollTextClass.DrawScrollingText(Sender: TObject);
begin
  if Active then
    Canvas.Draw(0,0,FBuffer);
end;

procedure TScrollTextClass.DoTimer(Sender: TObject);
var
  w: integer;
  s: string;
  i: integer;
begin
  if not Active then
    Exit;

  Dec(FOffset, FStepSize);

  if FOffSet < 0 then
    FStartLine := -FOffset div FLineHeight
  else
    FStartLine := 0;

  FEndLine := FStartLine + FNumLines + 1;
  if FEndLine > FLines.Count - 1 then
    FEndLine := FLines.Count - 1;

  FBuffer.Canvas.FillRect(Rect(0, 0, FBuffer.Width, FBuffer.Height));

  for i := FEndLine downto FStartLine do
  begin
    s := Trim(FLines[i]);

    //reset buffer font
    FBuffer.Canvas.Font:=fFont;
    FBuffer.Canvas.Font.Style := [];
    FBuffer.Canvas.Font.Color := clBlack;

    //skip empty lines
    if Length(s) > 0 then
    begin
      //check for bold format token
      if s[1] = '#' then
      begin
        s := copy(s, 2, Length(s) - 1);
        FBuffer.Canvas.Font.Style := [fsBold];
      end
      else
      begin
        //check for url
        if Pos('http://', s) = 1 then
        begin
          if i = FActiveLine then
          begin
            FBuffer.Canvas.Font.Style := [fsUnderline];
            FBuffer.Canvas.Font.Color := clRed;
          end
          else
            FBuffer.Canvas.Font.Color := clBlue;
         end;
      end;

      w := FBuffer.Canvas.TextWidth(s);
      FBuffer.Canvas.TextOut((FBuffer.Width - w) div 2, FOffset + i * FLineHeight, s);
    end;
  end;

  //start showing the list from the start
  if FStartLine > FLines.Count - 1 then
    FOffset := FBuffer.Height;
  Invalidate;
end;

function TScrollTextClass.ActiveLineIsURL: boolean;
begin
  if (FActiveLine > 0) and (FActiveLine < FLines.Count) then
    Result := Pos('http://', FLines[FActiveLine]) = 1
  else
    Result := False;
end;

procedure TScrollTextClass.DoOnChangeBounds;
begin
  inherited DoOnChangeBounds;
  Init;
end;

procedure TScrollTextClass.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);

  if ActiveLineIsURL then
    OpenURL(FLines[FActiveLine]);
end;

procedure TScrollTextClass.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);

  //calculate what line is clicked from the mouse position
  FActiveLine := (Y - FOffset) div FLineHeight;

  Cursor := crDefault;

  if (FActiveLine >= 0) and (FActiveLine < FLines.Count) and ActiveLineIsURL then
    Cursor := crHandPoint;
end;

constructor TScrollTextClass.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle + [csOpaque];

  OnPaint := @DrawScrollingText;
  FLines := TStringList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.OnTimer:=@DoTimer;
  FTimer.Interval:=30;
  FBuffer := TBitmap.Create;
  FFont:=TFont.Create;
  FFont.Size:=10;
  fBackColor:=clWindow;

  FStepSize := 1;
  FStartLine := 0;
  FOffset := -1;
  Width:=100;
  Height:=100;
  fTextFileName:=C_TEXTFILENAME;
  fVersionString:=C_VERSION;
  SendToBack;
end;

destructor TScrollTextClass.Destroy;
begin
  FLines.Free;
  FTimer.Free;
  FBuffer.Free;
  FFont.Free;
  inherited Destroy;
end;

end.
