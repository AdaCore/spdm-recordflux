with Channel;
with RFLX.SPDM_Responder.Session;
with RFLX.RFLX_Types;
with SPDM_C_Responder;

package body Responder_Select with
   SPARK_Mode,
   Refined_State => (Responder_State => (Contexts))
is

   type Index is range 1 .. 2;
   type Context_Array is array (Index) of SPDM_C_Responder.Context;
   Contexts : Context_Array;

   function Uninitialized return Boolean is
      (for all C of Contexts => RFLX.SPDM_Responder.Session.Uninitialized (C));

   procedure Responder_Main with
      Pre    => Uninitialized,
      Post   => Uninitialized,
      Global => (In_Out => Contexts);

   procedure Main
   is
   begin
      if Uninitialized then
         Responder_Main;
      end if;
   end Main;

   procedure Responder_Main with
      SPARK_Mode
   is
      use RFLX.RFLX_Types;

      package SR renames RFLX.SPDM_Responder.Session;

      generic
         with procedure Send (Buffer : Bytes);
         with procedure Receive (Buffer : out Bytes; Length : out RFLX.RFLX_Types.Length);
      procedure Run_Responder (Context : in out SPDM_C_Responder.Context) with
         Pre =>
            SR.Initialized (Context),
         Post =>
            SR.Initialized (Context);

      -- The `Run_Responder` procedure has the following tasks:
      --
      --    1. Bring the state machine in a state where data is needed, if that is not already the case.
      --    2. Provide the received data to the state machine.
      --    3. Bring the state machine in a state where all actions are done (e.g., sending a response).
      --       This is the case as soon as the state machine reaches a state where data is needed again.
      --
      -- If the state machine is not active or reaches a final state, not all of these tasks may be
      -- executed.
      procedure Run_Responder (Context : in out SPDM_C_Responder.Context) is
         Data_Received : Boolean := False;
         Buffer : Bytes (RFLX.RFLX_Types.Index'First .. RFLX.RFLX_Types.Index'First + 4095) := (others => 0);
         Length : RFLX.RFLX_Types.Length;
      begin
         pragma Assert (SR.Initialized (Context));

         while SR.Active (Context) and (not SR.Needs_Data (Context, SR.C_Transport) or not Data_Received) loop
            pragma Loop_Invariant (SR.Initialized (Context));

            if SR.Has_Data (Context, SR.C_Transport) then
               declare
                  use type RFLX.RFLX_Types.Length;
                  BS : constant RFLX.RFLX_Types.Length := SR.Read_Buffer_Size (Context, SR.C_Transport);
               begin
                  if Buffer'Length >= BS then
                     SR.Read
                        (Context,
                         SR.C_Transport,
                         Buffer (Buffer'First .. Buffer'First + BS - 1));
                     Send (Buffer (Buffer'First .. Buffer'First + BS - 1));
                  end if;
               end;
            end if;

            SR.Run (Context);

            if SR.Needs_Data (Context, SR.C_Transport) and not Data_Received then
               declare
                  BS : constant RFLX.RFLX_Types.Length := SR.Write_Buffer_Size (Context, SR.C_Transport);
               begin
                  Receive (Buffer, Length);
                  if Length in 1 .. BS then
                     SR.Write
                        (Context,
                         SR.C_Transport,
                         Buffer (Buffer'First .. Buffer'First + Length - 1));
                  end if;
               end;
               Data_Received := True;
            end if;
         end loop;

         pragma Assert (SR.Initialized (Context));
      end Run_Responder;

      package Channel_1 is new Channel;
      package Channel_2 is new Channel;

      procedure Run_Responder_1 is new Run_Responder (Channel_1.Send, Channel_1.Receive);
      procedure Run_Responder_2 is new Run_Responder (Channel_2.Send, Channel_2.Receive);

      function Has_Message return Index;

      -- Dummy implementation of a select-like function, which returns the index for the corresponding
      -- channel/responder when a message is available
      function Has_Message return Index is
         (Index'First)
      with
         SPARK_Mode => Off;

   begin
      --  https://github.com/Componolit/RecordFlux/issues/1032
      --  for Context of Contexts loop
      --     Context.Plat_Initialize;
      --  end loop;
      for I in Index loop
         SR.Initialize (Contexts (I));
      end loop;

      loop
         pragma Loop_Invariant (SR.Initialized (Contexts (1)));
         pragma Loop_Invariant (SR.Initialized (Contexts (2)));

         while (for all I in Index => SR.Active (Contexts (I))) loop
            pragma Loop_Invariant (SR.Initialized (Contexts (1)));
            pragma Loop_Invariant (SR.Initialized (Contexts (2)));

            --  Block until a message is received
            case Has_Message is
               when 1 =>
                  Run_Responder_1 (Contexts (1));
               when 2 =>
                  Run_Responder_2 (Contexts (2));
            end case;

            --  Execute application code here as required
         end loop;

         pragma Assert (SR.Initialized (Contexts (1)));
         pragma Assert (SR.Initialized (Contexts (2)));

         for I in Index loop
            if not SR.Active (Contexts (I)) then
               SR.Finalize (Contexts (I));
               SR.Initialize (Contexts (I));
            end if;
         end loop;
      end loop;
   end Responder_Main;

end Responder_Select;
