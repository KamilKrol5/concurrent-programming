with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Command_Line;


package body objects is
   package IO renames Ada.Text_IO;
   package FRand renames Ada.Numerics.Float_Random;

   task body machine is
      operator : operatorT;
      t : Float;
   begin
      accept Create (op : in operatorT) do
         operator := op;
      end Create;
      loop 
         accept DelegateTask (tsk : in out taskk) do
            if (tsk.operator /= operator) then
               IO.Put_Line("ERROR! An attempt to perfrom operation which is different from machine operation type.");
            else
               delay duration(machine_sleep);
               t := doTask(tsk);
            end if;
         end DelegateTask;
      end loop;
   end machine;
   
   
   task body employee is
      package R is new Ada.Numerics.Discrete_Random(MachinesListRange);
      Gen2 : R.Generator;
      taskToDo : taskk;
      newProduct : product;
      thisEmployeeRecordIndex : Integer;
      chosenMachineIndex : MachinesListRange;
   begin
      accept Start (index : in Integer) do
         thisEmployeeRecordIndex := index;
      end Start;
      loop
         R.Reset(Gen2);
         tasks.Remove(taskToDo);
         chosenMachineIndex := R.Random(Gen2);
         if employeeRecords(thisEmployeeRecordIndex).isPatient then
            inform("EMPLOYEE (PATIENT): Waiting for machine.");
            machinesSet(taskToDo.operator)(chosenMachineIndex).DelegateTask(taskToDo);
            newProduct := (value => taskToDo.result);
         else
            loop 
               inform("EMPLOYEE (IMPATIENT): Waiting for machine.");
               select 
                  machinesSet(taskToDo.operator)(chosenMachineIndex).DelegateTask(taskToDo);
                  newProduct := (value => taskToDo.result);
                  exit;
               or
                  delay Duration(IMPATIENT_WAIT);
                  inform("EMPLOYEE (IMPATIENT): I'll try another machine.");
               end select;
            end loop;
         end if;
         
         
         storage.Insert(newProduct);
         employeeRecords(thisEmployeeRecordIndex).numberOfTaskDone.Increment;
         inform("EMPLOYEE: I've done my task! Result is:" & Float'Image(newProduct.value));
         delay duration(employee_sleep);
      end loop;
   end employee;
   
   protected body ProtectedCounter is 
      function Get return Integer is
      begin
         return value;
      end;
      procedure Increment is 
      begin
         value := value + 1;
      end;
   end ProtectedCounter;
   
   
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
                      operators(Integer(R.Random(Gen2))),0.0);
         inform("CHAIRMAN: I've made up a new task! Trying to add it to task list.");
         tasks.Insert(newTask);
         inform("CHAIRMAN: I've added a new task to the task list.");
         delay duration(chairman_sleep);
      end loop;
   end chairman;
   
   
   task body client is
      newProduct : product;
   begin
      loop
         inform("CLIENT: I am waiting for my product.");
         storage.Remove(newProduct);
         inform("CLIENT: Product taken from display, product value: " & Float'Image(newProduct.value));
         delay duration(client_sleep);
      end loop;
   end client;
   
   
   
   
   procedure inform(message : String) is
   begin
      if (modee = TALKATIVE) then
         IO.Put_Line(message);
      end if;
   end inform;

   function doTask(tsk : in out  taskk) return Float is
   begin
      case tsk.operator is
         when '+' => tsk.result := tsk.first + tsk.second; return tsk.result;
         when '-' => tsk.result := tsk.first - tsk.second; return tsk.result;
         when '*' => tsk.result := tsk.first * tsk.second; return tsk.result;
      end case;

   end doTask;
   
   procedure printTaksArray(arr : TaskArray) is
   begin
      IO.Put("[ ");
      for I in arr'Range loop

         IO.Put("{" & Float'Image(arr(I).first) & operatorT'Image(arr(I).operator) & Float'Image(arr(I).second) & " }");

      end loop;
      IO.Put("]\n");
   end printTaksArray;
   
   procedure printStorageArrat(arr : StorageArray) is
   begin
      IO.Put("[ ");
      for I in arr'Range loop

         IO.Put(Float'Image(arr(I).value) & " ");

      end loop;
      IO.Put("]\n");
   end printStorageArrat;

   
end objects;
