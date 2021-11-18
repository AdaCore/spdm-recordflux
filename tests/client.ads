with GNAT.Sockets;

package Client is

   subtype Connection_Type is GNAT.Sockets.Socket_Type;

   procedure Connect (Port       :     GNAT.Sockets.Port_Type;
                      Connection : out Connection_Type);

end Client;
