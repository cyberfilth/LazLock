{%MainUnit eceditbtns.pas}

{**************************************************************************************************
 This file is part of the Eye Candy Controls (EC-C)
  This is *include* file, shared code of TECSpeedBtn and TECBitnBtn, belongs to eceditbtns.pas

  Copyright (C) 2018-2020 Vojtěch Čihák, Czech Republic

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

destructor BTNCLASSNAME.Destroy;
var aState: TItemState;
begin
  for aState in caEnabledStates do
    FreeAndNil(BtnBitmaps[aState]);
  inherited Destroy;
end;

procedure BTNCLASSNAME.BeginUpdate;
begin
  inc(UpdateCount);
end;

procedure BTNCLASSNAME.CalculatePreferredSize(var PreferredWidth,
            PreferredHeight: Integer; WithThemeSpace: Boolean);
var aCaption: string;
    aCaptionSize, aGlyphSize: TSize;
    aMargin: SmallInt;
begin
  aCaption := Caption;
  if aCaption <> '' then
    begin
      DeleteAmpersands(aCaption);
      aCaptionSize := Canvas.TextExtent(aCaption);
    end else
    aCaptionSize := Size(0, 0);
  aGlyphSize:=Size(-1, -1);
  if HasValidImages
    then aGlyphSize := Images.SizeForPPI[ImageWidth, Font.PixelsPerInch]
    else if HasValidActImage
           then aGlyphSize := TCustomAction(Action).ActionList.Images.SizeForPPI[ImageWidth, Font.PixelsPerInch]
           else aGlyphSize := Canvas.GlyphExtent(GlyphDesign);
  if RealLayout in [eopRight, eopLeft] then
    begin
      if aGlyphSize.cx*aCaptionSize.cx > 0 then inc(aGlyphSize.cx, Spacing);
      inc(aGlyphSize.cx, aCaptionSize.cx);
      if aCaptionSize.cx > 0 then inc(aGlyphSize.cx, 2*Spacing);
      aGlyphSize.cy := Math.max(aGlyphSize.cy, aCaptionSize.cy);
    end else
    begin
      aGlyphSize.cx := Math.max(aGlyphSize.cx, aCaptionSize.cx);
      if aGlyphSize.cy*aCaptionSize.cy > 0 then inc(aGlyphSize.cy, Spacing);
      inc(aGlyphSize.cy, aCaptionSize.cy);
    end;
  aMargin := Margin;
  if aMargin < 0 then aMargin := cBtnMargin;
  inc(aGlyphSize.cx, 2*aMargin);
  inc(aGlyphSize.cy, 2*aMargin);
  aGlyphSize.cx := aGlyphSize.cx or 1;  { Odd size glyphs look better }
  aGlyphSize.cy := aGlyphSize.cy or 1;
  PreferredWidth := aGlyphSize.cx;
  PreferredHeight := aGlyphSize.cy;
end;

procedure BTNCLASSNAME.Click;
begin
  case Mode of
    ebmToggleBox: if not assigned(Action) then Checked := not Checked;
    ebmDelayBtn: if Delay > 0 then
                   if Checked then
                     begin
                       ECTimer.Enabled := False;
                       ECTimer.Enabled := True;
                     end else
                     Checked := True;
  end;
  inherited Click;
end;

procedure BTNCLASSNAME.CMBiDiModeChanged(var Message: TLMessage);
var aRealLayout: TObjectPos;
begin
  aRealLayout := Layout;
  if IsRightToLeft then
    case aRealLayout of
      eopRight: aRealLayout := eopLeft;
      eopLeft: aRealLayout := eopRight;
    end;
  RealLayout := aRealLayout;
  Redraw;
end;

procedure BTNCLASSNAME.CMButtonPressed(var Message: TLMessage);
var aSender: BTNCLASSNAME;
begin
  if csDestroying in ComponentState then exit;
  if Message.WParam = WParam(FGroupIndex) then
    begin
      aSender := BTNCLASSNAME(Message.LParam);
      if aSender <> self then
        begin
          if aSender.Checked and FChecked then
            begin
              FChecked := False;
              Invalidate;
            end;
          FAllowAllUp := aSender.AllowAllUp;
        end;
    end;
end;

procedure BTNCLASSNAME.CMColorChanged(var Message: TLMessage);
begin
  NeedRedraw := True;
end;

procedure BTNCLASSNAME.CMParentColorChanged(var Message: TLMessage);
begin
  inherited CMParentColorChanged(Message);
  if not ParentColor then NeedRedraw := True;
end;

procedure BTNCLASSNAME.CreateTimer;
begin
  ECTimer := TCustomECTimer.Create(self);
  ECTimer.Enabled := False;
end;

function BTNCLASSNAME.DialogChar(var Message: TLMKey): Boolean;
begin
  Result := False;
  if Message.Msg = LM_SYSCHAR then
    begin
      if IsEnabled and IsVisible then
        begin
          if IsAccel(Message.CharCode, Caption) then
            begin
              Click;
              Result := True;
            end else
            Result := inherited DialogChar(Message);
        end;
    end;
end;

procedure BTNCLASSNAME.DrawButtonBMPs;
var aImageIndex, aImgIdxChckd, aLength: Integer;
    aCaption: string;
    aFlags: Cardinal;
    aGlyphDesign, aGlyphDsgnChckd: TGlyphDesign;
    aGlyphSize, aTextSize: TSize;
    aGlyphPoint: TPoint;
    aImages: TCustomImageList;
    aRect: TRect;
    aState: TItemState;
    aValidStates: TItemStates;

  procedure AdjustRect(AWidthLimit, AHeightLimit: SmallInt);
  var i, j, aLimit: SmallInt;
  begin
    i := -4;
    aLimit := aRect.Right - aRect.Left;
    if aLimit <= 21 then inc(i);
    aLimit := aLimit - AWidthLimit;
    if aLimit > 0 then dec(i, aLimit div 5);
    j := -4;
    aLimit := aRect.Bottom - aRect.Top - AHeightLimit;
    if aLimit > 0 then dec(j, aLimit div 5);
    InflateRect(aRect, i, j);
  end;

begin
  {$IFDEF DBGCTRLS} DebugLn('TCustomECxBtn.DrawButton'); {$ENDIF}
  if assigned(Parent) then
    begin
      if (csDesigning in ComponentState) and assigned(Action) and
        (TCustomAction(Action).ImageIndex >= 0) and not
        assigned(TCustomAction(Action).ActionList.Images) then ActionListImagesDelay:=True;
      aRect := ClientRect;
      aValidStates := ValidStates;
      for aState in aValidStates do
        begin
          BtnBitmaps[aState].TransparentColor := GetColorResolvingDefault(Color, Parent.Brush.Color);
          BtnBitmaps[aState].TransparentClear;
        end;
      if (Mode = ebmButton) and Flat then
        begin
          if eisEnabled in aValidStates then BtnBitmaps[eisHighlighted].Canvas.DrawButtonBackground(aRect, eisEnabled);
          aValidStates := aValidStates - [eisDisabled, eisEnabled, eisChecked, eisHighlighted];
        end;
      for aState in aValidStates do
        BtnBitmaps[aState].Canvas.DrawButtonBackground(aRect, aState);
      aValidStates := ValidStates;
      if not assigned(OnDrawGlyph) then
        begin
          aCaption := Caption;
          if (aCaption <> '') and ShowCaption then
            begin
              DeleteAmpersands(aCaption);
              aFlags := DT_CENTER or DT_VCENTER;
              Canvas.Font.Style:=Canvas.Font.Style+[fsBold, fsItalic, fsUnderline];
              with ThemeServices do
                aRect := GetTextExtent(Canvas.Handle, GetElementDetails(caThemedContent[eisEnabled]),
                  aCaption, aFlags, nil);
              aTextSize.cx := aRect.Right - aRect.Left + 1;
              aTextSize.cy := aRect.Bottom - aRect.Top + 1;
            end else
            aTextSize := Size(0, 0);
          if DropDownGlyph > edgNone then
            begin
              aGlyphSize := Canvas.GlyphExtent(egdArrowDown);
              if not IsRightToLeft
                then aGlyphPoint.X := Width - aGlyphSize.cx - cBtnDropDownGlyphIndent
                else aGlyphPoint.X := cBtnDropDownGlyphIndent;
              if DropDownGlyph = edgMiddle
                then aGlyphPoint.Y := (Height - aGlyphSize.cy + 2) div 2
                else aGlyphPoint.Y := Height - aGlyphSize.cy - cBtnDropDownGlyphIndent;
              aRect := Rect(aGlyphPoint.X, aGlyphPoint.Y, aGlyphPoint.X + aGlyphSize.cx,
                aGlyphPoint.Y + aGlyphSize.cy);
              for aState in aValidStates do
                BtnBitmaps[aState].Canvas.DrawGlyph(aRect, GlyphColor, egdArrowDown, aState);
            end;
          if assigned(Images) then
	          begin
              aImages:=Images;
              aImageIndex := ImageIndex;
              aImgIdxChckd := ImageIndexChecked;
              aGlyphSize := aImages.SizeForPPI[ImageWidth, Font.PixelsPerInch];
              if (aImageIndex >= 0) and (aImageIndex < aImages.Count) then
                begin
                  if ((aImgIdxChckd < 0) or (aImgIdxChckd >= aImages.Count)) and (Mode <> ebmButton)
                    then aImgIdxChckd := aImageIndex;
                end else
                if (aImgIdxChckd >= 0) and (aImgIdxChckd < aImages.Count) and (Mode = ebmButton)
                  then aImageIndex := aImgIdxChckd
                  else aImageIndex := -1;
            end else
            if HasValidActImage then
              begin
                aImages := TCustomAction(Action).ActionList.Images;
                aImageIndex := TCustomAction(Action).ImageIndex;
                aImgIdxChckd := aImageIndex;
                aGlyphSize := aImages.SizeForPPI[ImageWidth, Font.PixelsPerInch];
              end else
              begin
                aImageIndex := -1;
                aImgIdxChckd := -1;
              end;
          if (aImageIndex + aImgIdxChckd) <= -2 then
            begin
              aGlyphDesign := GlyphDesign;
              aGlyphDsgnChckd := GlyphDesignChecked;
              if aGlyphDsgnChckd = egdNone then aGlyphDsgnChckd := aGlyphDesign;
              if (aGlyphDesign = egdNone) and (Mode = ebmButton) then aGlyphDesign := GlyphDesignChecked;
              aGlyphSize := Canvas.GlyphExtent(aGlyphDesign);
              if aGlyphDesign > egdNone then
                begin
                  aGlyphSize.cx := aGlyphSize.cx or 1;
                  aGlyphSize.cy := aGlyphSize.cy or 1;
                end;
            end;
          aRect := ClientRect;
          if (aTextSize.cx > 0) and not AutoSize and (Margin >= 0) and (Layout in [eopRight, eopLeft]) then
            if not IsRightToLeft xor (Layout = eopRight)
              then aRect.Right := aTextSize.cx + 2 * Margin
              else aRect.Left := aRect.Right - aTextSize.cx - 2 * Margin;
          if aGlyphSize.cx*aTextSize.cx <> 0 then
            begin
              if RealLayout in [eopTop, eopBottom] then
                begin
                  aLength := aGlyphSize.cy + aTextSize.cy;
                  aGlyphPoint.X := (Width - aGlyphSize.cx) div 2;
                end else
                begin
                  aLength := aGlyphSize.cx + aTextSize.cx;
                  aGlyphPoint.Y := (Height - aGlyphSize.cy) div 2;
                end;
              inc(aLength, Spacing);
              case RealLayout of
                eopTop:
                  begin
                    aGlyphPoint.Y := (Height - aLength) div 2;
                    aRect.Top := aGlyphPoint.Y + aGlyphSize.cy + Spacing;
                  end;
                eopRight:
                  begin
                    if Margin < 0
                      then aRect.Left := (Width - aLength) div 2
                      else aRect.Left := Width - aLength - Margin;
                    aGlyphPoint.X := aRect.Left + aTextSize.cx + Spacing;
                  end;
                eopBottom:
                  begin
                    aRect.Top := (Height - aLength) div 2;
                    aGlyphPoint.Y := aRect.Top + aTextSize.cy + Spacing;
                  end;
                eopLeft:
                  begin
                    if Margin < 0
                      then aGlyphPoint.X := (Width - aLength) div 2
                      else aGlyphPoint.X := Margin;
                    aRect.Left := aGlyphPoint.X + aGlyphSize.cx + Spacing;
                  end;
              end;
              if RealLayout in [eopRight, eopLeft]
                then aRect.Right := aRect.Left + aTextSize.cx
                else aRect.Bottom := aRect.Top + aTextSize.cy;
            end else
            aGlyphPoint := Point((Width - aGlyphSize.cx) div 2, (Height - aGlyphSize.cy) div 2);
          if aTextSize.cx > 0 then
            begin
              if UseRightToLeftReading then aFlags := aFlags or DT_RTLREADING;
              for aState in aValidStates do
                begin
                  BtnBitmaps[aState].Canvas.Font.Assign(Font);
                  if aState >= eisChecked then
                    begin
                      BtnBitmaps[aState].Canvas.Font.Style := CheckedFontStyles;
                      BtnBitmaps[aState].Canvas.Font.Color := CheckedFontColor;
                    end;
                  if BtnBitmaps[aState].Canvas.Font.Color = clDefault
                    then BtnBitmaps[aState].Canvas.Font.Color := clBtnText;
                  with ThemeServices do
                    DrawText(BtnBitmaps[aState].Canvas, GetElementDetails(caThemedContent[aState]),
                      Caption, aRect, aFlags, 0);
                end;
            end;
          if (aImageIndex + aImgIdxChckd) >= -1 then
            begin
              if Mode = ebmButton then
                begin
                  for aState in aValidStates do
                    aImages.ResolutionForPPI[ImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor].
                      Draw(BtnBitmaps[aState].Canvas, aGlyphPoint.X, aGlyphPoint.Y,
                      aImageIndex, caGraphEffects[aState]);
                end else
                begin
                  if aImageIndex >= 0 then
                    for aState in aValidStates*[eisDisabled, eisHighlighted, eisEnabled] do
                      aImages.ResolutionForPPI[ImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor].
                        Draw(BtnBitmaps[aState].Canvas, aGlyphPoint.X, aGlyphPoint.Y,
                        aImageIndex, caGraphEffects[aState]);
                  if aImgIdxChckd >= 0 then
                    for aState in aValidStates*[eisChecked, eisPushed, eisPushedHilighted, eisPushedDisabled] do
                      aImages.ResolutionForPPI[ImageWidth, Font.PixelsPerInch, GetCanvasScaleFactor].
                        Draw(BtnBitmaps[aState].Canvas, aGlyphPoint.X, aGlyphPoint.Y,
                        aImgIdxChckd, caGraphEffects[aState]);
                end;
            end else
            begin
              if aTextSize.cx = 0 then
                begin
                  case aGlyphDesign of
                    egdGrid..egdSizeArrLeft: AdjustRect(30, 30);
                    egdRectBeveled..high(TGlyphDesign): AdjustRect(85, 30);
                  end;
                end else
                aRect := Rect(aGlyphPoint.X, aGlyphPoint.Y, aGlyphPoint.X + aGlyphSize.cx, aGlyphPoint.Y + aGlyphSize.cy);
              if Mode = ebmButton then
                begin
                  if aGlyphDesign > egdNone then
                    for aState in aValidStates do
                      BtnBitmaps[aState].Canvas.DrawGlyph(aRect, GlyphColor, aGlyphDesign, aState);
                end else
                begin
                  if aGlyphDesign > egdNone then
                    for aState in aValidStates*[eisDisabled, eisHighlighted, eisEnabled] do
                      BtnBitmaps[aState].Canvas.DrawGlyph(aRect, GlyphColor, aGlyphDesign, aState);
                  if aGlyphDsgnChckd > egdNone then
                    for aState in aValidStates*[eisChecked, eisPushed, eisPushedHilighted, eisPushedDisabled] do
                      BtnBitmaps[aState].Canvas.DrawGlyph(aRect, GlyphColor, aGlyphDsgnChckd, aState);
                end;
            end;
        end else
        for aState in aValidStates do
          OnDrawGlyph(self, aState);
    end;
  NeedRedraw := False;
end;

procedure BTNCLASSNAME.EndUpdate;
begin
  dec(UpdateCount);
  if UpdateCount = 0 then
    begin
      NeedRedraw := True;
      if AutoSize then
        begin
          InvalidatePreferredSize;
          AdjustSize;
        end;
      Invalidate;
    end;
end;

procedure BTNCLASSNAME.FontChanged(Sender: TObject);
begin
  inherited FontChanged(Sender);
  NeedRedraw := True;  { Invalidate not necessary here }
end;

function BTNCLASSNAME.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TECSpeedBtnActionLink;
end;

function BTNCLASSNAME.HasValidActImage: Boolean;
var aAction: TCustomAction;
begin
  aAction := TCustomAction(Action);
  Result := (assigned(aAction) and assigned(aAction.ActionList.Images) and
    (aAction.ImageIndex >= 0) and (aAction.ImageIndex < aAction.ActionList.Images.Count));
end;

function BTNCLASSNAME.HasValidImages: Boolean;
begin
  Result := (assigned(Images) and ((ImageIndex >= 0) or (ImageIndexChecked >= 0))
    and ((ImageIndex < Images.Count) or (ImageIndexChecked < Images.Count)));
end;

procedure BTNCLASSNAME.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
    begin
      BtnPushed := True;
      if (Mode = ebmButton) and assigned(ECTimer) then
        if (Repeating = 0) or assigned(OnRepeating) then ECTimer.Enabled := True;
    end;
  Invalidate;
end;

procedure BTNCLASSNAME.MouseLeave;
begin
  {$IFDEF DBGCTRLS} DebugLn('TBaseECxBtn.MouseLeave'); {$ENDIF}
  inherited MouseLeave;
  if BtnPushed then MouseUp(mbLeft, [ssLeft], 0, 0);
  Invalidate;
end;

procedure BTNCLASSNAME.MouseEnter;
begin
  inherited MouseEnter;
  Invalidate;
end;

procedure BTNCLASSNAME.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  BtnPushed := False;
  if Mode <> ebmDelayBtn then  { when Delay <> 0 SetChecked cares of Invalidate; }
    begin
      if assigned(ECTimer) then ECTimer.Enabled := False;
      if Mode = ebmButton then Invalidate;
    end;
end;

procedure BTNCLASSNAME.Redraw;
begin
  NeedRedraw := True;
  if UpdateCount = 0 then Invalidate;
end;

procedure BTNCLASSNAME.Resize;
var aState: TItemState;
    aWidth, aHeight: Integer;
begin
  inherited Resize;
  aWidth := Width;
  aHeight := Height;
  if (aWidth <> PrevSize.cx) or (aHeight <> PrevSize.cy) then
    begin
      if PrevSize.cx = -1 then
        begin
          for aState in caEnabledStates do
            begin
              BtnBitmaps[aState] := TBitmap.Create;
              BtnBitmaps[aState].SetProperties(aWidth, aHeight, Transparent);
            end;
          BtnBitmaps[eisDisabled] := BtnBitmaps[eisHighlighted];
          BtnBitmaps[eisPushedDisabled] := BtnBitmaps[eisPushedHilighted];
        end else
        for aState in caEnabledStates do
          BtnBitmaps[aState].SetSize(aWidth, aHeight);
      Redraw;
      PrevSize.cx := aWidth;
      PrevSize.cy := aHeight;
    end;
end;

procedure BTNCLASSNAME.ResizeInvalidate;
begin
  if UpdateCount = 0 then
    begin;
      if AutoSize then
        begin
          InvalidatePreferredSize;
          AdjustSize;
        end;
      Invalidate;
    end;
end;

procedure BTNCLASSNAME.SetAction(Value: TBasicAction);
begin
  inherited SetAction(Value);
  if assigned(Value) and (Value is TCustomAction) and TCustomAction(Value).AutoCheck then Delay := -1;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure BTNCLASSNAME.SetAutoSize(Value: Boolean);
begin
  inherited SetAutoSize(Value);
  if Value then
    begin
      InvalidatePreferredSize;
      AdjustSize;
      NeedRedraw := True;
      Invalidate;
    end;
end;

procedure BTNCLASSNAME.SetParent(NewParent: TWinControl);
begin
  inherited SetParent(NewParent);
  NeedRedraw := True;
end;

procedure BTNCLASSNAME.SetTimerEvent;
begin
  if Mode = ebmDelayBtn
    then ECTimer.OnTimer := @TimerOnTimerDelay
    else if Repeating = 0
           then ECTimer.OnTimer := @TimerOnTimerHold
           else ECTimer.OnTimer := @TimerOnTimerRepeating;
end;

procedure BTNCLASSNAME.TextChanged;
begin
  inherited TextChanged;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure BTNCLASSNAME.TimerOnTimerDelay(Sender: TObject);
begin
  ECTimer.Enabled := False;
  Checked := False;
  Invalidate;
end;

procedure BTNCLASSNAME.TimerOnTimerHold(Sender: TObject);
begin
  ECTimer.Enabled := False;
  if assigned(OnHoldDown) then
    begin
      OnHoldDown(self);
      ControlState := ControlState - [csClicked];
    end;
end;

procedure BTNCLASSNAME.TimerOnTimerRepeating(Sender: TObject);
begin
  if assigned(OnRepeating) then
    begin
      OnRepeating(self);
      ControlState := ControlState - [csClicked];
    end;
end;

procedure BTNCLASSNAME.UpdateGroup;
var aMsg : TLMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) and (not (csLoading in ComponentState)) then
  begin
    aMsg.Msg := CM_ButtonPressed;
    aMsg.WParam := FGroupIndex;
    aMsg.LParam := PtrInt(self);
    aMsg.Result := 0;
    Parent.Broadcast(aMsg);
  end;
end;

{ BTNCLASSNAME.Setters }

procedure BTNCLASSNAME.SetAllowAllUp(AValue: Boolean);
begin
  if FAllowAllUp = AValue then exit;
  FAllowAllUp := AValue;
  UpdateGroup;
end;

procedure BTNCLASSNAME.SetChecked(AValue: Boolean);
begin
  if (FChecked = AValue) or (not AValue and (GroupIndex <> 0) and (not AllowAllUp)) then exit;
  FChecked := AValue;
  if [csLoading, csDestroying]*ComponentState = [] then
    begin
      if GroupIndex <> 0 then UpdateGroup;
      if AValue then
        begin
          case Mode of
            ebmButton: Mode := ebmToggleBox;
            ebmDelayBtn: if not (csDesigning in ComponentState)
                           then ECTimer.Enabled := True
                           else Mode := ebmToggleBox;
          end;
        end;
      if assigned(OnChange) then OnChange(self);
      if not AValue and assigned(OnRelease) then OnRelease(self);
    end;
  if not AValue or not BtnDrawnPushed then Invalidate;
end;

procedure BTNCLASSNAME.SetCheckedFontColor(AValue: TColor);
begin
  if FCheckedFontColor=AValue then exit;
  FCheckedFontColor:=AValue;
  Redraw;
end;

procedure BTNCLASSNAME.SetCheckedFontStyles(AValue: TFontStyles);
begin
  if FCheckedFontStyles=AValue then exit;
  FCheckedFontStyles:=AValue;
  Redraw;
end;

procedure BTNCLASSNAME.SetDelay(AValue: Integer);
begin
  if FDelay = AValue then exit;
  FDelay := AValue;
  if not (csDesigning in ComponentState) then
    begin
      if (AValue > 0) and (Mode <> ebmToggleBox) then
        begin
          if not assigned(ECTimer) then
            begin
              CreateTimer;
              SetTimerEvent;
            end;
          ECTimer.Delay := AValue;
          ECTimer.Repeating := Repeating;
        end else
        FreeAndNil(ECTimer);
    end;
end;

procedure BTNCLASSNAME.SetDropDownGlyph(AValue: TDropDownGlyph);
begin
  if FDropDownGlyph = AValue then exit;
  FDropDownGlyph := AValue;
  Redraw;
end;

procedure BTNCLASSNAME.SetFlat(AValue: Boolean);
begin
  if FFlat=AValue then exit;
  FFlat:=AValue;
  Redraw;
end;

procedure BTNCLASSNAME.SetGlyphColor(AValue: TColor);
begin
  if FGlyphColor = AValue then exit;
  FGlyphColor := AValue;
  if not (HasValidImages or HasValidActImage) then Redraw;
end;

procedure BTNCLASSNAME.SetGlyphDesign(AValue: TGlyphDesign);
begin
  if FGlyphDesign=AValue then exit;
  FGlyphDesign:=AValue;
  if not (HasValidImages or HasValidActImage) then
    begin
      NeedRedraw := True;
      ResizeInvalidate;
    end;
end;

procedure BTNCLASSNAME.SetGlyphDesignChecked(AValue: TGlyphDesign);
begin
  if FGlyphDesignChecked = AValue then exit;
  FGlyphDesignChecked := AValue;
  if not (HasValidImages or HasValidActImage) and
    ((Mode <> ebmButton) or (GlyphDesign = egdNone)) then Redraw;
end;

procedure BTNCLASSNAME.SetGroupIndex(AValue: Integer);
begin
  if FGroupIndex = AValue then exit;
  FGroupIndex := AValue;
  if AValue <> 0 then
    begin
      FDelay := -1;  { only checkable button makes sense in group }
      UpdateGroup;
    end;
end;

procedure BTNCLASSNAME.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex = AValue then exit;
  FImageIndex := AValue;
  if assigned(Images) then
    begin
      NeedRedraw := True;
      ResizeInvalidate;
    end;
end;

procedure BTNCLASSNAME.SetImageIndexChecked(AValue: TImageIndex);
begin
  if FImageIndexChecked = AValue then exit;
  FImageIndexChecked := AValue;
  if (Mode <> ebmButton) or (ImageIndex < 0) then Redraw;
end;

procedure BTNCLASSNAME.SetImages(AValue: TCustomImageList);
begin
  if FImages = AValue then exit;
  FImages := AValue;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure BTNCLASSNAME.SetImageWidth(AValue: SmallInt);
begin
  if FImageWidth = AValue then exit;
  if AValue < 0 then AValue := 0;
  FImageWidth := AValue;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure BTNCLASSNAME.SetLayout(AValue: TObjectPos);
begin
  if FLayout = AValue then exit;
  FLayout := AValue;
  if IsRightToLeft then
    case AValue of
      eopRight: AValue := eopLeft;
      eopLeft: AValue := eopRight;
    end;
  RealLayout := AValue;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure BTNCLASSNAME.SetMargin(AValue: SmallInt);
begin
  if FMargin = AValue then exit;
  FMargin := AValue;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure BTNCLASSNAME.SetMode(AValue: TButtonMode);
begin
  if FMode = AValue then exit;
  if (AValue = ebmButton) or (FMode = ebmButton) and ((ImageIndex <> ImageIndexChecked)
    or (GlyphDesign <> GlyphDesignChecked)) then NeedRedraw := True;
  FMode := AValue;
  if AValue <> ebmToggleBox then
    begin
      FGroupIndex := 0;
      Checked := False;
      if Delay > 0 then
        begin
          if not assigned(ECTimer) then CreateTimer;
          ECTimer.Delay := Delay;
          ECTimer.Repeating := Repeating;
        end;
    end else
    FreeAndNil(ECTimer);
  if assigned(ECTimer) then SetTimerEvent;
end;

procedure BTNCLASSNAME.SetRepeating(AValue: Integer);
begin
  if FRepeating = AValue then exit;
  FRepeating := AValue;
  if assigned(ECTimer) then
    begin
      ECTimer.Repeating := AValue;
      SetTimerEvent;
    end;
end;

procedure BTNCLASSNAME.SetShowCaption(AValue: Boolean);
begin
  if FShowCaption = AValue then exit;
  FShowCaption := AValue;
  if Caption <> '' then
    begin
      NeedRedraw := True;
      ResizeInvalidate;
    end;
end;

procedure BTNCLASSNAME.SetSpacing(AValue: SmallInt);
begin
  if FSpacing = AValue then exit;
  FSpacing := AValue;
  NeedRedraw := True;
  ResizeInvalidate;
end;

procedure BTNCLASSNAME.SetTransparent(AValue: Boolean);
var aState: TItemState;
begin
  if FTransparent = AValue then exit;
  FTransparent := AValue;
  for aState in ValidStates do
    BtnBitmaps[aState].Transparent := AValue;
  Redraw;
end;


