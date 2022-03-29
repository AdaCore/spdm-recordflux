package Responder_Select with
   SPARK_Mode,
   Abstract_State => Responder_State,
   Initializes => Responder_State,
   Initial_Condition => Uninitialized
is
   pragma Annotate (GNATprove, False_Positive,
                    """Contexts"" constituent of ""Responder_State"" is not initialized",
                    "ISSUE: Componolit/RecordFlux#954");

   function Uninitialized return Boolean with
      Ghost;

   procedure Main with
      Pre    => Uninitialized,
      Post   => Uninitialized,
      Global => (In_Out => Responder_State);

end Responder_Select;
