with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Command_Line;
use Ada.Text_IO;

procedure Main is
   package FRand renames Ada.Numerics.Float_Random;

   type Mode is (CALM,TALKATIVE);
   type operatorT is ('+', '-', '*');
   type operatorsArray is array(0..2) of operatorT;

   MAX_ARGUMENT_VALUE :  constant  := 500.0;
   MAX_EMPLOYEES        : constant  := 2;
   MAX_CHAIRMEN         : constant  := 1;
   MAX_CLIENTS          : constant  := 1;
   MAX_TASKLIST_SIZE    : constant  := 40;
   MAX_STORAGE_CAPACITY : constant  := 40;
   EMPLOYEE_SLEEP       : constant  := 1000;
   CHAIRMAN_SLEEP       : constant  := 400;
   CLIENT_SLEEP         : constant  := 2000;

   modee : Mode := CALM;
   operators : operatorsArray := ('+', '-', '*');

   type taskk is record
      first : Float;
      second : Float;
      operator : operatorT;
   end record;

   type product is record
      value : Float;
   end record;

   type StorageListRange is new Positive range 1 .. MAX_STORAGE_CAPACITY;
   type StorageArray is array(StorageListRange) of product;

   type TaskListRange is new Positive range 1 .. MAX_TASKLIST_SIZE;
   type TaskArray is array(TaskListRange) of taskk;

   protected type TaskBufferType is
      entry Insert (An_Item : in  taskk);
      entry Remove (An_Item : out taskk);
   private
      Length : Natural range 0 .. MAX_TASKLIST_SIZE := 0;
      Head, Tail : TaskListRange := 1;
      Data : TaskArray;
   end TaskBufferType;

   protected body TaskBufferType is
      entry Insert (An_Item : in taskk)
        when Length < MAX_TASKLIST_SIZE is
      begin
         Data(Tail) := An_Item;
         Tail := Tail mod MAX_TASKLIST_SIZE + 1;
         Length := Length + 1;
      end Insert;
      entry Remove (An_Item : out taskk)
        when Length > 0 is
      begin
         An_Item := Data(Head);
         Head := Head mod MAX_TASKLIST_SIZE + 1;
         Length := Length - 1;
      end Remove;
   end TaskBufferType;

   tasks : TaskBufferType;

   protected type StorageBufferType is
      entry Insert (An_Item : in  product);
      entry Remove (An_Item : out product);
   private
      Length : Natural range 0 .. MAX_TASKLIST_SIZE := 0;
      Head, Tail : StorageListRange := 1;
      Data : StorageArray;
   end StorageBufferType;

   protected body StorageBufferType is
      entry Insert (An_Item : in product)
        when Length < MAX_STORAGE_CAPACITY is
      begin
         Data(Tail) := An_Item;
         Tail := Tail mod MAX_STORAGE_CAPACITY + 1;
         Length := Length + 1;
      end Insert;
      entry Remove (An_Item : out product)
        when Length > 0 is
      begin
         An_Item := Data(Head);
         Head := Head mod MAX_STORAGE_CAPACITY + 1;
         Length := Length - 1;
      end Remove;
   end StorageBufferType;

   storage: StorageBufferType;

   procedure inform(message : String) is
   begin
      if (modee = TALKATIVE) then
         Put_Line(message);
      end if;
   end inform;

   function doTask(tsk : taskk) return Float is
   begin
      case tsk.operator is
         when '+' => return tsk.first + tsk.second;
         when '-' => return tsk.first - tsk.second;
         when '*' => return tsk.first * tsk.second;
      end case;

   end doTask;

   task chairman;
   task body chairman is
      newTask : taskk;
      Gen : FRand.Generator;
      type A is range operatorsArray'First..operatorsArray'Last;
      package R is new Ada.Numerics.Discrete_Random(A);
      Gen2 : R.Generator;
   begin
      FRand.Reset(Gen);
      R.Reset(Gen2);
      loop
         newTask :=  (FRand.Random(Gen) * MAX_ARGUMENT_VALUE,
                      FRand.Random(Gen) * MAX_ARGUMENT_VALUE,
                      operators(Integer(R.Random(Gen2))));
         inform("CHAIRMAN: I've made up a new task! Trying to add it to task list.");
         tasks.Insert(newTask);
         inform("CHAIRMAN: I've added a new task to the task list.");
         delay(Duration(CHAIRMAN_SLEEP));
      end loop;
   end chairman;


   --     tests
   --     type My_Array is array(5..12) of Float;
   --     Arr : My_Array := (7 => 9.6, 11 => 99.0, others => 1.1);
   --     type MyInt is new Integer range -100..100;
   --     subtype MyInt2 is Integer range -100..100;
begin
--     ada.Command_Line.Argument(1)
   Put_Line("xD");
   null;
end Main;
