with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Assertions;      use Ada.Assertions;
with Ada.Numerics.Discrete_Random;
with Ada.Exceptions;      use Ada.Exceptions;

with SkipListPkg; use SkipListPkg;

procedure Test_Skip_List is

   -- Helper functions for generating large random inputs:

   type Int_Array is array (Natural range <>) of Integer;

   function compareInts (I, J : Integer) return Integer is
   begin
      if I < J then
         return -1;
      elsif I > J then
         return 1;
      else
         return 0;
      end if;
   end compareInts;

   procedure swap (I, J : in out Integer) is
      Temp : Integer := I;
   begin
      I := J;
      J := Temp;
   end swap;

   procedure Shuffle (Arr : in out Int_Array) is
      Num_Swaps : constant Integer := Arr'Length * 10;
      subtype Interval is Integer range Arr'Range;
      package Rand_Index is new Ada.Numerics.Discrete_Random
        (Result_Subtype => Interval);
      use Rand_Index;
      Gen            : Rand_Index.Generator;
      Index1, Index2 : Integer;
   begin
      for I in 1 .. Num_Swaps loop
         Reset (Gen);
         Index1 := Random (Gen);
         Reset (Gen);
         Index2 := Random (Gen);
         swap (Arr (Index1), Arr (Index2));
      end loop;
   end Shuffle;

   function Random_Array (Max : Integer) return Int_Array is
      Arr : Int_Array (1 .. Max);
   begin
      for I in 1 .. Max loop
         Arr (I) := I;
      end loop;
      Shuffle (Arr);
      return Arr;
   end Random_Array;

   procedure Put (Arr : Int_Array) is
   begin
      for I of Arr loop
         Put (I, Width => 0);
         New_Line;
      end loop;
   end Put;

   -- Helper functions for common assertions.
   -- Help ensure that height, size, and isEmpty are in sync.
   -- and give more detailed debugging information.

   procedure Assert_Height (List : Skip_List; Expected_Height : Natural) is
   begin
      Assert
        (Height (List) = Expected_Height,
         "Expected " & Expected_Height'Image & " but found " &
         Height (List)'Image & " when testing for height of list");
   end Assert_Height;

   procedure Assert_Size (List : Skip_List; Expected_Size : Natural) is
   begin
      Assert
        (Size (List) = Expected_Size,
         "Expected " & Expected_Size'Image & " but found " &
         Size (List)'Image & " when testing for size of list");
   end Assert_Size;

   procedure Assert_Empty (List : Skip_List) is
   begin
      Assert (Is_Empty (List), "Expected list to be empty, but it wasn't!");
      Assert_Height (List, 0);
      Assert_Size (List, 0);
   end Assert_Empty;

   -- Inserts the item, but then does some extra checking to
   -- ensure height and size are correct.
   procedure Assert_Insert
     (List        : in out Skip_List; Item : Integer;
      Node_Height :        Natural := Random_Height)
   is
      Old_Size   : Natural := Size (List);
      Old_Height : Natural := Height (List);
   begin
      Insert (List => List, Item => Item, New_Node_Height => Node_Height);
      Assert (not Is_Empty (List), "List was empty after insertion!");
      Assert
        (Size (List) = Old_Size + 1,
         "Wrong size after insertion of value " & Item'Image);
      Assert
        (Height (List) = Integer'Max (Old_Height, Node_Height),
         "Wrong height after insertion of value " & Item'Image);

   end Assert_Insert;

   -- Removes the item, but then does some extra checking to
   -- ensure height and size are correct.
   procedure Assert_Remove (List : in out Skip_List; Item : Integer) is
      Old_Size   : Natural := Size (List);
      Old_Height : Natural := Height (List);
   begin
      Assert (Remove (List => List, Item => Item));
      Assert
        (Size (List) = Old_Size - 1,
         "Wrong size after removal of value " & Item'Image);

      -- Does not ensure new height is exactly correct, but
      -- might catch some issues.
      Assert
        (Height (List) <= Old_Height,
         "Height increased by removing value " & Item'Image);
   end Assert_Remove;

   -- Repeatedly removes Item until it doesn't exist any more.
   procedure Assert_Remove_All_Occurrences
     (List : in out Skip_List; Item : Integer)
   is
      Old_Size   : Natural := Size (List);
      Old_Height : Natural := Height (List);
   begin
      while Contains (List, Item) loop
         Assert_Remove (List, Item);
      end loop;
      Assert
        (not Remove (List, Item),
         " Expected remove to return false," & " but it returned true!");
   end Assert_Remove_All_Occurrences;

   -- Destroys skip list and ensures that list is empty.
   procedure Assert_Destroy (List : in out Skip_List) is
   begin
      Destroy (List);
      Assert_Empty (List);
   end Assert_Destroy;

   -- Adds all items in Arr to the List.
   -- Unless Use_Height_One is True, the height will be random.
   procedure Assert_Insert_All
     (List           : in out Skip_List; Arr : Int_Array;
      Check_Contains :    Boolean := True; Use_Height_One : Boolean := False)
   is
   begin
      for Val of Arr loop
         if Use_Height_One then
            Assert_Insert (List, Val, Node_Height => 1);
         else
            Assert_Insert (List, Val);
         end if;
         if Check_Contains then
            Assert
              (Contains (List, Val),
               "List doesn't contain value " & Val'Image &
               " after inserting it!");
         end if;
      end loop;
      if Check_Contains then
         for Val of Arr loop
            Assert
              (Contains (List, Val),
               "List doesn't contain value " & Val'Image &
               " after inserting items of Arr it!");
         end loop;
      end if;
   end Assert_Insert_All;

   -- Ensures the list contains all the items in Arr.
   procedure Assert_Contains_All (List : Skip_List; Arr : Int_Array) is
   begin
      for Val of Arr loop
         Assert
           (Contains (List, Val),
            "Expected list to contain value " & Val'Image & " but it didn't!");
      end loop;
   end Assert_Contains_All;

   -- Removes all the items in Arr from List.
   -- As items are removed, ensures list doesn't contain them.

   -- Note: assumes that all items in Skip_List are unique!
   procedure Assert_Remove_All (List : in out Skip_List; Arr : Int_Array) is
   begin
      for Val of Arr loop
         Assert_Remove (List, Val);
         Assert
           (not Contains (List, Val),
            "List still contains value " & Val'Image &
            " even after we removed it!" &
            " (Ensure test doesn't involve duplicates.");
      end loop;
   end Assert_Remove_All;

   -- For each item in Arr, continues removing occurences of the
   -- item until the list doesn't contain the item anymore.
   procedure Assert_Remove_All_Including_Duplicates
     (List : in out Skip_List; Arr : Int_Array)
   is
   begin
      for Val of Arr loop
         Assert_Remove_All_Occurrences (List => List, Item => Val);
      end loop;
   end Assert_Remove_All_Including_Duplicates;

   -- Call this to cancel a test.
   -- Will cause test to fail, but allowed you to skip
   -- long tests.
   procedure Suppress is
   begin
      Assert (False, "Suppressed");
   end Suppress;
   -------------- The tests --------------

   -- A very simple test that adds a single element,
   -- and makes sure size and height are set correctly.
   procedure Test_1 is
      List : Skip_List;
   begin
      Assert_Empty (List);
      Assert_Insert (List => List, Item => 1, Node_Height => 1);
   end Test_1;

   -- Tests insert and Put, using height of 1.
   -- Does not use contains.
   procedure Test_2 is
      List : Skip_List;
      Arr  : Int_Array (1 .. 10) := (5, 6, 1, 10, 9, 4, 3, 2, 7, 8);
   begin
      Assert (not Contains (List, 1));
      Assert_Insert_All
        (List           => List, Arr => Arr, Check_Contains => False,
         Use_Height_One => True);
      -- Requires manual verification.
      Put ("Output of Put(List) :");
      Put (List);
      New_Line;
   end Test_2;

   -- Tests insert and contains using height of 1.
   procedure Test_3 is
      List : Skip_List;
      Arr  : Int_Array (1 .. 10) := (10, 1, 2, 3, 9, 8, 7, 5, 4, 6);
   begin
      Assert_Empty (List);
      Assert_Insert_All
        (List           => List, Arr => Arr, Check_Contains => True,
         Use_Height_One => True);
   end Test_3;

   -- Inserts several values, but with different heights.
   -- Output of detailed put must be manually verified.
   procedure Test_4 is
      List    : Skip_List;
      Arr     : Int_Array := (10, 1, 2, 3, 9, 8, 7, 5, 4, 6);
      Heights : Int_Array := (3, 2, 1, 5, 2, 1, 10, 1, 4, 3);
   begin
      for Index in Arr'Range loop
         Assert_Insert (List, Arr (Index), Node_Height => Heights (Index));
      end loop;
      Put ("Output of Put(List) :");
      New_Line;
      Put (List);
      New_Line;
      Put ("Output of Detailed_Put(List):");
      New_Line;
      Detailed_Put (List);
   end Test_4;

   procedure Test_5 is
      List    : Skip_List;
      Arr     : Int_Array := (5, 6, 1, 10, 9, 4, 3, 2, 7, 8);
      Heights : Int_Array := (1, 2, 3, 4, 1, 2, 3, 4, 1, 2);
   begin
      for Index in Arr'Range loop
         Assert_Insert (List, Arr (Index), Node_Height => Heights (Index));
         Assert
           (Contains (List, Arr (Index)),
            "List doesn't contain value " & Arr (Index)'Image &
            " after inserting it!");
      end loop;
   end Test_5;

   -- Giant random stress tests for inputs of size 100_000.
   -- Insert and contains only.
   procedure Test_6 is
      List : Skip_List;
      Arr  : Int_Array := Random_Array (100_000);
   begin
      Assert_Insert_All
        (List           => List, Arr => Arr, Check_Contains => True,
         Use_Height_One => False);
   end Test_6;

   -- Very simple remove test (for some edges cases)
   procedure Test_7 is
      List : Skip_List;
   begin
      Assert_Empty (List);
      Assert_Insert (List => List, Item => 1, Node_Height => 1);
      Assert (Contains (List, 1), "Doesn't contain value after insertion!");
      Assert_Remove (List => List, Item => 1);
      Assert (not Contains (List, 1), "Value wasn't removed!");
      Assert_Empty (List);
   end Test_7;

   -- Insert a few items, then remove them. Height 1 only.
   procedure Test_8 is
      List : Skip_List;
      Arr  : Int_Array := (5, 6, 1, 10, 9, 4, 3, 2, 7, 8);
   begin
      Assert_Insert_All
        (List           => List, Arr => Arr, Check_Contains => True,
         Use_Height_One => True);
      Assert_Remove_All_Including_Duplicates (List => List, Arr => Arr);
      Assert_Empty (List);
   end Test_8;

   -- Insert a few items, then remove them. Variable height this time.
   procedure Test_9 is
      List : Skip_List;
      Arr  : Int_Array := (5, 6, 1, 10, 9, 4, 3, 2, 7, 8);
   begin
      Assert_Insert_All
        (List           => List, Arr => Arr, Check_Contains => True,
         Use_Height_One => False);
      Assert_Remove_All_Including_Duplicates (List => List, Arr => Arr);
      Assert_Empty (List);
   end Test_9;

   -- Add huge # of items, then remove them all in random order.
   -- Use random heights. Might time out if skip list is
   -- not implemented with right efficiency.
   procedure Test_10 is
      List : Skip_List;
      Arr  : Int_Array := Random_Array (100_000);
   begin
      Assert_Insert_All
        (List           => List, Arr => Arr, Check_Contains => True,
         Use_Height_One => False);
      Assert_Remove_All_Including_Duplicates (List => List, Arr => Arr);
   end Test_10;

   -- Large random stress tests with many copies of each number.
   -- Each copy will have a random height.
   procedure Test_11 is
      List : Skip_List;
      Arr  : Int_Array := Random_Array (1_000);
   begin
      -- Insert each item in Array 1_000 times
      -- Each time, order of insertion is random.
      for I in 1 .. 100 loop
         Assert_Insert_All
           (List           => List, Arr => Arr, Check_Contains => True,
            Use_Height_One => False);
         Shuffle (Arr);
      end loop;
      Assert_Remove_All_Including_Duplicates (List => List, Arr => Arr);
   end Test_11;

   -- Memory leak test. Repeatedly add many items and then remove them.
   -- If deallocation is done improperly, program will probably
   -- run out of heap space.
   -- This may take a while...
   -- No timeout should be enforced!
   procedure Test_12 is
      List                  : Skip_List;
      Arr                   : Int_Array := Random_Array (10);
      Percentage_Completion : Integer   := 0;
   begin
      Put ("This may take a while ... ");
      New_Line;
      for I in 1 .. 100_000 loop
         Assert_Insert_All (List, Arr);
         Assert_Remove_All_Including_Duplicates (List, Arr);
         if I mod 10_000 = 0 then
            Percentage_Completion := Percentage_Completion + 10;
            Put ("Test is " & Percentage_Completion'Image & " % complete");
            New_Line;
         end if;
      end loop;
   end Test_12;

   -- Destroy memory leak test.
   -- Repeatedly add many items and then calls destroy.
   -- If deallocation is done improperly, program will probably
   -- run out of heap space.
   -- This may take a while...
   procedure Test_13 is
      List : Skip_List;
      Arr  : Int_Array := Random_Array (100_000);
   begin
      for I in 1 .. 10 loop
         Assert_Insert_All
           (List           => List, Arr => Arr, Check_Contains => True,
            Use_Height_One => False);
         Assert_Destroy (List);
      end loop;
   end Test_13;

   -- Corner case for memory leak detection.
   -- Repeadetly makes a single GIANT tower, and deallocates it.
   procedure Test_14 is
      List                  : Skip_List;
      Percentage_Completion : Integer := 0;
   begin
      Put ("This may take a while...");
      New_Line;
      for I in 1 .. 10_000 loop
         Assert_Insert (List => List, Item => 1, Node_Height => 1_000);
         Assert_Remove (List => List, Item => 1);
         Assert_Empty (List);
         if I mod 1_000 = 0 then
            Percentage_Completion := Percentage_Completion + 10;
            Put ("Test is " & Percentage_Completion'Image & " % complete");
            New_Line;
         end if;
      end loop;
   end Test_14;

   -- An extra test. Currently unused. Use if you want.
   procedure Test_Insert_1 is
      List : Skip_List;
   begin
      -- Empty
      Detailed_Put (List);
      Assert_Empty (List);
      Insert (List => List, Item => 5, New_Node_Height => 1);
      Assert_Height (List, 1);
      Assert_Size (List, 1);
      -- 5
      Detailed_Put (List);

      Insert (List => List, Item => 1, New_Node_Height => 1);
      Assert_Height (List, 1);
      Assert_Size (List, 2);

      -- 1           5
      Detailed_Put (List);

      Insert (List => List, Item => 2, New_Node_Height => 2);
      Assert_Height (List, 2);
      Assert_Size (List, 3);

      --             2
      -- 1           2           5
      Detailed_Put (List);

      Insert (List => List, Item => 7, New_Node_Height => 1);
      Assert_Height (List, 2);
      Assert_Size (List, 4);

      --             2
      -- 1           2           5          7
      Detailed_Put (List);

      Insert (List => List, Item => 2, New_Node_Height => 2);
      Assert_Height (List, 2);
      Assert_Size (List, 5);

      --        2     2
      -- 1      2     2           5          7
      Detailed_Put (List);

      Insert (List => List, Item => 4, New_Node_Height => 3);
      Assert_Height (List, 3);
      Assert_Size (List, 6);
      --                    4
      --        2     2     4
      -- 1      2     2     4      5          7
      Detailed_Put (List);

      Insert (List => List, Item => 6, New_Node_Height => 2);
      Assert_Height (List, 3);
      Assert_Size (List, 7);

      --                    4
      --        2     2     4           6
      -- 1      2     2     4      5    6      7
      Detailed_Put (List);

      Insert (List => List, Item => 3, New_Node_Height => 1);
      Assert_Height (List, 3);
      Assert_Size (List, 8);

      --                      4
      --        2       2     4           6
      -- 1      2   3   2     4      5    6      7
      Detailed_Put (List);
   end Test_Insert_1;

   -- Runs a test, and outputs to console whether it passed or failed.
   type ProcPtr is access procedure;
   function Test_With_Success_Detection
     (Test : ProcPtr; Name : String) return Boolean
   is
   begin
      Put ("Running test " & Name & " :");
      New_Line;
      Put ("Result : ");
      Test.all; -- Runs the procedure Test
      Put ("PASS");
      New_Line;
      New_Line;
      return True; -- No assertion error exception thrown.
   exception
      when Error : others =>
         Put ("FAIL : ");
         Put ("Exception: ");
         Put (Exception_Name (Error));
         Put (" with message : ");
         Put (Exception_Message (Error));
         New_Line;
         New_Line;
         return False;
   end Test_With_Success_Detection;

   -- Runs all tests, and documents which succeeded and which failed.
   -- If you fail a test, you can test it in isolation and examine the
   -- exact source of the assertion error within the test.
   procedure Test_All_With_Results_Summary is
      type ProcPtrArr is array (Natural range <>) of ProcPtr;
      Tests         : ProcPtrArr (1 .. 14) :=
        (Test_1'Access, Test_2'Access, Test_3'Access, Test_4'Access,
         Test_5'Access, Test_6'Access, Test_7'Access, Test_8'Access,
         Test_9'Access, Test_10'Access, Test_11'Access, Test_12'Access,
         Test_13'Access, Test_14'Access);
      Success_Count : Natural              := 0;
   begin
      for Test_Index in Tests'Range loop
         if Test_With_Success_Detection
             (Test => Tests (Test_Index), Name => "Test #" & Test_Index'Image)
         then
            Success_Count := Success_Count + 1;
         end if;
      end loop;
      Put
        ("Total Tests Passed : " & Success_Count'Image & " /" &
         Tests'Last'Image);
      New_Line;
      New_Line;
      Put
        ("Note: Correctness of output for Put and Detailed_Put must be " &
         "verified manually!!");
      New_Line;
      New_Line;
      Put ("Note: Tests are necessarily worth equal points!");
      New_Line;
      New_Line;
      Put ("Note: Final grading may use additional tests!");
      New_Line;
      New_Line;
   end Test_All_With_Results_Summary;
begin
   Test_All_With_Results_Summary;
end Test_Skip_List;
