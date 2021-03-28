{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit poweredby;

interface

uses
  uPoweredby, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uPoweredby', @uPoweredby.Register);
end;

initialization
  RegisterPackage('poweredby', @Register);
end.
