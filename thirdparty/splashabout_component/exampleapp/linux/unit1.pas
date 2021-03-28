unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, usplashabout;

type

  { TForm1 }

  TForm1 = class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    SplashAbout1: TSplashAbout;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
    SplashAbout1.ShowSplash;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  SplashAbout1.ShowAbout;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
    SplashAbout1.ShowSplash;

end;

end.

