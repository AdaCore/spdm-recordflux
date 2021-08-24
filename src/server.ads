with GNAT.Sockets;

package Server is

   subtype Server_Type is GNAT.Sockets.Socket_Type;

   subtype Connection_Type is GNAT.Sockets.Socket_Type;

   function Create_Server return Server_Type;

   procedure Listen (Socket     : in out Server_Type;
                     Port       :        GNAT.Sockets.Port_Type;
                     Connection : out    Connection_Type);

end Server;
