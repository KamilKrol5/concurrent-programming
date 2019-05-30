with Ada.Text_IO;
with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Ada.Command_Line;
with constants;
with objects;
--  with Ada.Strings.Unbounded;
--  use Ada.Strings.unbounded;


use objects;
use constants;


procedure Main is
   package IO renames Ada.Text_IO;
   package FRand renames Ada.Numerics.Float_Random;
   Gen : FRand.Generator;
begin
   if (Ada.Command_Line.Argument_Count < 1) then
      IO.Put_Line("Wrong number of arguments. Valid input is calm|-c|talkative|-t");
   else
      if (Ada.Command_Line.Argument(1) = "-t" or Ada.Command_Line.Argument(1) = "talkative") then
         modee := TALKATIVE;
      elsif (Ada.Command_Line.Argument(1) = "-c" or Ada.Command_Line.Argument(1) = "calm") then
         modee := CALM;
      end if;
   end if;




   for k in machinesSet'Range loop
       for p in machinesSet(k)'Range loop
          machinesSet(k)(p).Create(k, WORKING);
       end loop;
   end loop;

   for k in ServiceMenListRange loop
      serviceMen(k).id := Integer(k);
   end loop;

   FRand.Reset(Gen);
   for j in employeeRecords'Range loop
      employeeRecords(j).isPatient := FRand.Random(Gen) < IMPATIENT_PROBABILITY;
      employees(j).Start(j);
   end loop;

   if (modee = CALM) then
      loop
         IO.Put_Line("Options:" & NL & "    s - show Storage"& NL &"    t - show Task list" & NL &"    e - show Employees statistics" & NL);
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
            IO.Get_Line(S, Last);
             if (S(1..Last) = "s") then
               storageArr := storage.seeStorage(Length,HeadS);
               IO.Put("[ ");
               for I in Natural(HeadS)..(Natural(HeadS) + Length -1) loop
                  IO.Put(Float'Image(storageArr(StorageListRange(I mod Length + 1)).value) & " ");
               end loop;
               IO.Put("]" & NL);
             elsif (S(1..Last) = "t") then
               tasksArr := tasks.seeTaskList(Length,HeadT);
               IO.Put("[ ");
               for I in Natural(HeadT)..(Natural(HeadT) + Length -1) loop
                  index := TaskListRange(I mod Length + 1);
                  IO.Put("{" & Float'Image(tasksArr(index).first) &
                        operatorT'Image(tasksArr(index).operator) &
                        Float'Image(tasksArr(index).second) & " } ");
               end loop;
               IO.Put("]" & NL);
             elsif (S(1..Last) = "e") then
               IO.Put("[ " & NL);
               for I in employeeRecords'Range loop
                  IO.Put_Line("{ isPatient: " & Boolean'Image(employeeRecords(I).isPatient) &
                                " number of tasks done" & Integer'Image(employeeRecords(I).numberOfTaskDone.Get) &" } ");
               end loop;
               IO.Put("]" & NL);
            else
               IO.Put_Line("Wrong argument");
            end if;
         end;
      end loop;
   end if;
   IO.Put_Line("xD");

end Main;
