with GNAT.Sockets;

package Server is

   subtype Server_Type is GNAT.Sockets.Socket_Type;

   subtype Connection_Type is GNAT.Sockets.Socket_Type;

   function Create_Server return Server_Type;

   procedure Bind (Socket : in out Server_Type;
                   Port   :        GNAT.Sockets.Port_Type;
                   Addr   :    out GNAT.Sockets.Sock_Addr_Type);

   procedure Listen (Socket     : in out Server_Type;
                     Addr       : in out GNAT.Sockets.Sock_Addr_Type;
                     Connection :    out Connection_Type);

end Server;
