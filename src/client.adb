package body Client is

   procedure Connect (Port       :     GNAT.Sockets.Port_Type;
                      Connection : out Connection_Type)
   is
      Address : GNAT.Sockets.Sock_Addr_Type;
   begin
      GNAT.Sockets.Create_Socket (Connection);
      Address.Addr := GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name ("localhost"), 1);
      Address.Port := Port;
      GNAT.Sockets.Set_Socket_Option (Connection, GNAT.Sockets.Socket_Level, (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Connect_Socket (Connection, Address);
   end Connect;

end Client;
