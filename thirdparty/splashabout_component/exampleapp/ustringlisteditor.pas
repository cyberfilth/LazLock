unit uStringListEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { Tfrm_StringListEditor }

  Tfrm_StringListEditor = class(TForm)
    cmd_ClearMemo: TBitBtn;
    cmd_Close: TBitBtn;
    StringListEditorMemo: TMemo;
    procedure cmd_ClearMemoClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frm_StringListEditor: Tfrm_StringListEditor;

implementation
{$R *.lfm}

{ Tfrm_StringListEditor }

procedure Tfrm_StringListEditor.cmd_ClearMemoClick(Sender: TObject);
begin
    StringListEditorMemo.Clear;
end;

end.

