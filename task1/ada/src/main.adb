with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Command_Line;
--  with Ada.Strings.Unbounded;
--  use Ada.Strings.unbounded;
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
   EMPLOYEE_SLEEP       : constant  := 1;
   CHAIRMAN_SLEEP       : constant  := 0.4;
   CLIENT_SLEEP         : constant  := 2;
   NL : constant String := Character'Val(13) & Character'Val(10);

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
      function seeTaskList(len : out Natural;head1 : out TaskListRange) return TaskArray;
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
      function seeTaskList(len : out Natural;head1 : out TaskListRange) return TaskArray is
      begin
         len := Length;
         head1 := Head;
         return Data;
      end seeTaskList;
   end TaskBufferType;

   tasks : TaskBufferType;

   protected type StorageBufferType is
      entry Insert (An_Item : in  product);
      entry Remove (An_Item : out product);
      function seeStorage(len : out Natural;head1 : out StorageListRange) return StorageArray;
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
      function seeStorage(len : out Natural;head1 : out StorageListRange) return StorageArray is
      begin
         len := Length;
         head1 := Head;
        return Data;
      end seeStorage;
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

   procedure printTaksArray(arr : TaskArray) is
   begin
      Put("[ ");
      for I in arr'Range loop

            Put("{" & Float'Image(arr(I).first) & operatorT'Image(arr(I).operator) & Float'Image(arr(I).second) & " }");

      end loop;
      Put("]\n");
   end printTaksArray;

   procedure printStorageArrat(arr : StorageArray) is
   begin
      Put("[ ");
      for I in arr'Range loop

            Put(Float'Image(arr(I).value) & " ");

      end loop;
      Put("]\n");
   end printStorageArrat;

   task type chairman;
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
   type chairman_array is array(1..MAX_CHAIRMEN) of chairman;
   chairmen : chairman_array;

   task type employee;
   task body employee is
      taskToDo : taskk;
      newProduct : product;
   begin
      loop
         tasks.Remove(taskToDo);
         newProduct := (value => doTask(taskToDo));
         storage.Insert(newProduct);
         inform("EMPLOYEE: I've done my task! Result is:" & Float'Image(newProduct.value));
         delay(Duration(EMPLOYEE_SLEEP));
      end loop;
   end employee;
   type employee_array is array(1..MAX_EMPLOYEES) of employee;
   employees : employee_array;

   task type client;
   task body client is
      newProduct : product;
   begin
      loop
         inform("CLIENT: I am waiting for my product.");
         storage.Remove(newProduct);
         inform("CLIENT: Product taken from display, product value: " & Float'Image(newProduct.value));
         delay(Duration(CLIENT_SLEEP));
      end loop;
   end client;
   type clients_array is array(1..MAX_CLIENTS) of client;
   clients : clients_array;







begin
   if (Ada.Command_Line.Argument_Count < 2) then
      Put_Line("Wrong number of arguments. Valid input is calm|-c|talkative|-t");
   else
      if (Ada.Command_Line.Argument(2) = "-t" or Ada.Command_Line.Argument(2) = "talkative") then
         modee := TALKATIVE;
      elsif (Ada.Command_Line.Argument(2) = "-c" or Ada.Command_Line.Argument(2) = "calm") then
         modee := CALM;
      end if;
   end if;

   if (modee = CALM) then
      loop
         Put_Line("Options:" & NL & "    s - show Storage"& NL &"    t - show Task list" & NL);
         declare
            S : String(1..40);
            Last : Natural;

            HeadT : TaskListRange;
            HeadS : StorageListRange;
            Length : Natural;
            storageArr : StorageArray;
            tasksArr : TaskArray;
            index : TaskListRange;
         begin
            Get_Line(S, Last);
             if (S(1..Last) = "s") then
               storageArr := storage.seeStorage(Length,HeadS);
               Put("[ ");
               for I in Natural(HeadS)..(Natural(HeadS) + Length -1) loop
                  Put(Float'Image(storageArr(StorageListRange(I mod Length + 1)).value) & " ");
               end loop;
               Put("]" & NL);
             elsif (S(1..Last) = "t") then
               tasksArr := tasks.seeTaskList(Length,HeadT);
               Put("[ ");
               for I in Natural(HeadT)..(Natural(HeadT) + Length -1) loop
                  index := TaskListRange(I mod Length + 1);
                  Put("{" & Float'Image(tasksArr(index).first) &
                        operatorT'Image(tasksArr(index).operator) &
                        Float'Image(tasksArr(index).second) & " } ");
               end loop;
               Put("]" & NL);
            else
               Put_Line("Wrong argument");
            end if;
         end;
      end loop;
   end if;
   Put_Line("xD");

end Main;
