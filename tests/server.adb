package body Server
is

   function Create_Server return Server_Type
   is
      Socket : GNAT.Sockets.Socket_Type;
   begin
      GNAT.Sockets.Create_Socket (Socket);
      return Socket;
   end Create_Server;

   procedure Bind (Socket :     Server_Type;
                   Port   :     GNAT.Sockets.Port_Type;
                   Addr   : out GNAT.Sockets.Sock_Addr_Type)
   is
   begin
      Addr.Addr := GNAT.Sockets.Addresses (GNAT.Sockets.Get_Host_By_Name ("localhost"), 1);
      Addr.Port := Port;
      GNAT.Sockets.Set_Socket_Option (Socket, GNAT.Sockets.Socket_Level, (GNAT.Sockets.Reuse_Address, True));
      GNAT.Sockets.Bind_Socket (Socket, Addr);
      GNAT.Sockets.Listen_Socket (Socket);
   end Bind;

   procedure Listen (Socket     :        Server_Type;
                     Addr       : in out GNAT.Sockets.Sock_Addr_Type;
                     Connection :    out Connection_Type)
   is
   begin
      GNAT.Sockets.Accept_Socket (Socket, Connection, Addr);
   end Listen;

end Server;
