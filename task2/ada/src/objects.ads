with constants;
use constants;

package objects is
   
   type Mode is (CALM,TALKATIVE);
   type operatorT is ('+', '-', '*');
   type operatorsArray is array(0..2) of operatorT;
   NL : constant String := Character'Val(13) & Character'Val(10);
   
   protected type ProtectedCounter is 
      function Get return Integer;
      procedure Increment;
   private
      value : Integer := 0;
   end ProtectedCounter;
   
   type taskk is record
      first : Float;
      second : Float;
      operator : operatorT;
      result : Float;
   end record;

   type product is record
      value : Float;
   end record;

--     type machineRecord is record
--        operation: operatorT;
--     end record;
   
   task type machine is
      entry Create (op : in operatorT);
      entry DelegateTask (tsk : in out taskk);
   end machine;
   
   type employeeRecord is record
      isPatient: Boolean;
      numberOfTaskDone: ProtectedCounter;
   end record;
   
   task type employee is
      entry Start (index : in Integer);
   end employee;
   
   task type chairman;
   
   task type client;
   
   
   type StorageListRange is new Positive range 1 .. MAX_STORAGE_CAPACITY;
   type StorageArray is array(StorageListRange) of product;

   type TaskListRange is new Positive range 1 .. MAX_TASKLIST_SIZE;
   type TaskArray is array(TaskListRange) of taskk;

   type MachinesListRange is new Positive range 1 .. NUMBER_OF_MACHINES;
   type MachinesArray is array(MachinesListRange) of machine;
   type MachinesSetArray is array(operatorT) of MachinesArray;
   
   type chairman_array is array(1..MAX_CHAIRMEN) of chairman;
   type clients_array is array(1..MAX_CLIENTS) of client;
   type employee_array is array(1..MAX_EMPLOYEES) of employee;
   type employeeRecord_array is array(1..MAX_EMPLOYEES) of employeeRecord;
   
   procedure inform(message : String);
   function doTask(tsk : in out  taskk) return Float;
   procedure printTaksArray(arr : TaskArray);
   procedure printStorageArrat(arr : StorageArray);
   
   
   protected type TaskBufferType is
      entry Insert (An_Item : in  taskk);
      entry Remove (An_Item : out taskk);
      function seeTaskList(len : out Natural;head1 : out TaskListRange) return TaskArray;
   private
      Length : Natural range 0 .. MAX_TASKLIST_SIZE := 0;
      Head, Tail : TaskListRange := 1;
      Data : TaskArray;
   end TaskBufferType;

   
   protected type StorageBufferType is
      entry Insert (An_Item : in  product);
      entry Remove (An_Item : out product);
      function seeStorage(len : out Natural;head1 : out StorageListRange) return StorageArray;
   private
      Length : Natural range 0 .. MAX_TASKLIST_SIZE := 0;
      Head, Tail : StorageListRange := 1;
      Data : StorageArray;
   end StorageBufferType;
   
   modee : Mode := CALM;
   operators : operatorsArray := ('+', '-', '*');
   tasks : TaskBufferType;
   storage: StorageBufferType;  
   chairmen : chairman_array; 
   clients : clients_array;
   employeeRecords : employeeRecord_array;
   employees : employee_array;
   machinesSet : MachinesSetArray;

end objects;
