with Interfaces.C.Strings;

package body RFLX.RFLX_Debug is

   procedure Print (Message : String) is
      procedure C_Print (Message : Interfaces.C.Strings.chars_ptr) with
         Import,
         Convention => C,
         External_Name => "rflx_debug_print";
   begin
      C_Print (Interfaces.C.Strings.New_String (Message));
   end Print;

end RFLX.RFLX_Debug;
