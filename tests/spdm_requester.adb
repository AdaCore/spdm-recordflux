with Ada.Text_IO;
with Ada.Sequential_IO;
with Ada.Directories;
with Basalt.Strings_Generic;

package body SPDM_Requester
is

   function Byte_Image is new Basalt.Strings_Generic.Image_Modular (RFLX.RFLX_Types.Byte);
   package Byte_IO is new Ada.Sequential_IO (RFLX.RFLX_Types.Byte);

   overriding
   procedure Get_Request_Data (Ctx    : in out Context;
                               Result :    out RFLX.SPDM_Requester.Request.Structure)
   is
      use type RFLX.RFLX_Types.Index;
      File_Name : String := Request_Arg (Ctx.Index);
      File      : Byte_IO.File_Type;
      Index     : RFLX.RFLX_Types.Index := Result.Data'First;
   begin
      Ada.Text_IO.Put_Line ("Loading request from " & File_Name);
      Result.Size := RFLX.SPDM_Requester.Size (Ada.Directories.Size (File_Name));
      Byte_IO.Open (File, Byte_IO.In_File, File_Name);
      while not Byte_IO.End_Of_File (File) loop
         Byte_IO.Read (File, Result.Data (Index));
         Index := Index + 1;
      end loop;
      Byte_IO.Close (File);
      Ctx.Index := Ctx.Index + 1;
   end Get_Request_Data;

   overriding
   procedure Return_Response (Ctx    : in out Context;
                              Resp   :        RFLX.RFLX_Types.Bytes;
                              Result :    out Boolean)
   is
   begin
      Ada.Text_IO.Put ("Response:");
      for B of Resp loop
         Ada.Text_IO.Put (" " & Byte_Image (B, 16));
      end loop;
      Ada.Text_IO.New_Line;
      Result := Ctx.Index <= Last_Index;
   end Return_Response;

end SPDM_Requester;
