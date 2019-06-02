with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Command_Line;
with Ada.Exceptions;  use Ada.Exceptions;


package body objects is
   package IO renames Ada.Text_IO;
   package FRand renames Ada.Numerics.Float_Random;

   machinesServiceSet : MachinesServiceSetArray;
   
   task body machine is
      operator : operatorT;
      state: Integer;
      package FRand renames Ada.Numerics.Float_Random;
      Gen : FRand.Generator;
      t : Float;
      isBrokenRandom: Boolean;
   begin
      accept Create (op : in operatorT; initState : in Integer) do
         operator := op;
         state := initState;
      end Create;
      loop 
         select
         accept DelegateTask (tsk : in out taskk) do
            if (tsk.operator /= operator) then
               IO.Put_Line("ERROR! An attempt to perfrom operation which is different from machine operation type.");
            else
               if (state = WORKING) then
                  delay duration(machine_sleep);
                  t := doTask(tsk);
                  inform("MACHINE: Result sent.");
                  isBrokenRandom := FRand.Random(Gen) < BREAK_PROBABILITY;
                  if (isBrokenRandom) then
                     state := BROKEN;
                     inform("MACHINE BROKEN!");
                  end if;
               end if;
            end if;
            end DelegateTask;
          or
            accept fix do
               state := WORKING;
               IO.Put_Line("MACHINE: I am fixed!");
            end fix;
         or
            accept getStatus(stat : out Integer) do
               stat := state;
            end getStatus;
         end select;
      end loop;
   end machine;
   
   
   task body employee is
      package R is new Ada.Numerics.Discrete_Random(MachinesListRange);
      Gen2 : R.Generator;
      taskToDo : taskk;
      newProduct : product;
      thisEmployeeRecordIndex : Integer;
      chosenMachineIndex : MachinesListRange;
      taskIsDone: Boolean := False;
   begin
      accept Start (index : in Integer) do
         thisEmployeeRecordIndex := index;
      end Start;
      loop
         R.Reset(Gen2);
         tasks.Remove(taskToDo);
         
         if employeeRecords(thisEmployeeRecordIndex).isPatient then
            taskIsDone := False;
            while not taskIsDone loop
               chosenMachineIndex := R.Random(Gen2);
               inform("EMPLOYEE (PATIENT): Waiting for machine.");
               machinesSet(taskToDo.operator)(chosenMachineIndex).DelegateTask(taskToDo);
               if taskToDo.result = null then 
                  inform("PATIENT EMPLOYEE: I got an empty result. Machine must be broken.");
                  servicee.reportBrokenMachine(report'(taskToDo.operator, chosenMachineIndex));
               else
                  inform("PATIENT EMPLOYEE: I got a result.");
		  taskIsDone := true;
               end if;
            end loop;
            newProduct := (value => taskToDo.result.all);
         else
            taskIsDone := False;
            while not taskIsDone loop
               loop 
                  chosenMachineIndex := R.Random(Gen2);
                  inform("EMPLOYEE (IMPATIENT): Waiting for machine.");
                  select 
                     machinesSet(taskToDo.operator)(chosenMachineIndex).DelegateTask(taskToDo);
                     
                     exit;
                  or
                     delay Duration(IMPATIENT_WAIT);
                     inform("EMPLOYEE (IMPATIENT): I'll try another machine.");
                  end select;
               end loop;
               if taskToDo.result = null then 
                  inform("IMPATIENT EMPLOYEE: I got an empty result. Machine must be broken.");
                  servicee.reportBrokenMachine(report'(taskToDo.operator, chosenMachineIndex));
                  inform("IMPATIENT EMPLOYEE: I reported a broken machine.");
               else
                  inform("IMPATIENT EMPLOYEE: I got a result.");
                  taskIsDone := true;
               end if;
            end loop;
            newProduct := (value => taskToDo.result.all);
         end if;
         
         storage.Insert(newProduct);
         employeeRecords(thisEmployeeRecordIndex).numberOfTaskDone.Increment;
         inform("EMPLOYEE: I've done my task! Result is:" & Float'Image(newProduct.value));
         delay duration(employee_sleep);
      end loop;
   end employee;
   
   task body serviceMan is
      rep2 : report; 
      mId: ServiceMenListRange;
   begin
      loop
            accept goAndFixMachine(rep : in report; myIndex : ServiceMenListRange) do
               machinesServiceSet(rep.machineType)(rep.machineIndex).hasManAssigned := True;
               inform("SERVICE MAN " & Integer'Image(serviceMen(myIndex).id) & " was sent to fix a machine" );
               delay duration(SERVICE_MAN_SLEEP);
               inform("SERVICE MAN " & Integer'Image(serviceMen(myIndex).id) &  ": is fixing a machine" );
               machinesSet(rep.machineType)(rep.machineIndex).fix;
               rep2 := rep;
               mId := myIndex;
            end goAndFixMachine;
         servicee.fixReportEntry(fixReport'(rep2.machineType, rep2.machineIndex, mId));
         
      end loop;
   end serviceMan;
   
   task body service is
      tmpMan: serviceManRecord;
      tmpStat : Integer;
   begin
      loop
         select
            accept reportBrokenMachine(rep : in report) do
               if not machinesServiceSet(rep.machineType)(rep.machineIndex).hasManAssigned then
                  inform("SERVICE: I have received a report about broken machine");
                  machinesServiceSet(rep.machineType)(rep.machineIndex).status := BROKEN;
               end if;
            end reportBrokenMachine;
              or
            accept fixReportEntry(rep : in fixReport) do
               machinesServiceSet(rep.targetMachineType)(rep.tatgetMachineIndex).hasManAssigned := False;
               machinesServiceSet(rep.targetMachineType)(rep.tatgetMachineIndex).status := WORKING;
               serviceMen(rep.whoFixedIndex).isFree := True;
            end fixReportEntry; 
         end select;
         for op in machinesServiceSet'Range loop
            for m in machinesServiceSet(op)'Range loop
               machinesSet(op)(m).getStatus(tmpStat);
               if tmpStat = BROKEN then  
                  inform("SERVICE: I will try to find free service man");
                   loop1: for manIndex in serviceMen'Range loop
                     tmpMan := serviceMen(manIndex);
                     if tmpMan.isFree then
                        inform("SERVICE: free service man has been found and will be sent to a broken machine");
                        serviceMen(manIndex).isFree := False;
                        serviceMenTasks(ServiceMenTaskListRange(manIndex)).goAndFixMachine(report'(op, m), manIndex);
                        exit loop1;
                     end if; 
                  end loop loop1;
              end if;
            end loop;
         end loop;
      end loop;
   exception
      when Error: others =>
         Ada.Text_IO.Put ("Unexpected exception: ");
         Ada.Text_IO.Put_Line (Exception_Information(Error));
   end service;
   
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
                      operators(Integer(R.Random(Gen2))),null);
         --("CHAIRMAN: I've made up a new task! Trying to add it to task list.");
         tasks.Insert(newTask);
         --("CHAIRMAN: I've added a new task to the task list.");
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
         when '+' => tsk.result := new Float'(tsk.first + tsk.second); return tsk.result.all;
         when '-' => tsk.result := new Float'(tsk.first - tsk.second); return tsk.result.all;
         when '*' => tsk.result := new Float'(tsk.first * tsk.second); return tsk.result.all;
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
