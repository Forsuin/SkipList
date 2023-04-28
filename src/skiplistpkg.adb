with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Assertions;      use Ada.Assertions;
with Ada.Numerics.Discrete_Random;
package body SkipListPkg is
   -- Returns a Random_Height by repeadetly flipping a coin.
   -- until the result is true. Used to specify the
   -- Height of a newly inserted value if it is left
   -- unspecified.
   subtype Rand_Range is Integer range 0 .. 1;
   package Rand_Bit is new Ada.Numerics.Discrete_Random (Rand_Range);
   function Random_Height return Positive is
      use Rand_Bit;
      Gen    : Rand_Bit.Generator;
      Height : Positive := 1;
   begin
      Reset (Gen);
      while Random (Gen) = 1 loop
         Height := Height + 1;
         Reset (Gen);
      end loop;
      return Height;
   end Random_Height;

   -- You may implement additional helper functions / procedures
   -- within this package body
   -- as you see fit.

   -- nextRow = begin.below
   -- for each node in row
   --   free(node)
   -- begin = nextRow
   -- repeat
   procedure Destroy (List : in out Skip_List) is
   begin
      null;
   end Destroy;

   -- begin will be the first sentinel value in this row
   -- currentNode = begin
   -- while (currentNode.below)
   --   currentNode = currentNode.below
   -- while(currentNode != nullptr)
   --   Put(currentNode.data)
   procedure Put (List : Skip_List) is
      currentNode : Node_Ptr := List.Top_Left;
   begin
      while currentNode.Below /= null loop
         currentNode := currentNode.Below;
      end loop;
      -- point to first data node
      currentNode := currentNode.Right;
      while currentNode /= null loop
         Put (currentNode.Data'Image & ' ');
         currentNode := currentNode.Right;
      end loop;

   end Put;

   -- currentNode* = begin
   -- beginningOfRow* = &currentNode
   -- while(beginningOfRow != nullptr) {
   --   while(currentNode != nullptr)
   --     Put(currentNode.data)
   -- }
   procedure Detailed_Put (List : Skip_List) is
      currentNode : Node_Ptr;
      beginRow    : Node_Ptr := List.Top_Left;
   begin
      while beginRow /= null loop
         currentNode := beginRow.Right;
         while currentNode /= null loop
            Put (currentNode.Data'Image & ' ');
            currentNode := currentNode.Right;
         end loop;
         New_Line;
         beginRow := beginRow.Below;
      end loop;
   end Detailed_Put;

   -- Insert the Item into the List
   procedure Insert
     (List            : in out Skip_List; Item : Integer;
      New_Node_Height :        Positive := Random_Height)
   is
      -- Create a "tower" of the appropriate height for the
      -- new node.
      -- Update sentinel nodes to be at least as high as the new tower.
      -- Conduct a search. Start at top layer.
      -- Within each layer, scan right until we find the insertion
      -- point within the layer.
      -- Insert within the layer if and only if the tower is tall enough.
      -- Regardless, move down to next layer, and repeat.

      -- Other considerations:
      -- Update height and size appropriately.
      -- Handle the edge cases:
      --     Possible ones might be : Height / Size 0, Size 1, Height 1

      New_Node_Top : Node_Ptr :=
        new Node'(Data => Item, Right => null, Below => null);
   begin
      -- Update the sentinel tower's height to be tall enough to
      -- accomodate the new node.
      -- Logic should handle height 0 case.
      declare
         Sentinel_Tower_Top : Node_Ptr := List.Top_Left;
      begin
         while List.Height < New_Node_Height loop
            -- Increase height by 1 by adding a single sentinel node
            -- (which is not connected to anything)
            Sentinel_Tower_Top :=
              new Node'
                (Data  => Integer'First, -- Value should be unused.
                 Right => null,        --
                 Below => Sentinel_Tower_Top);
            List.Top_Left      := Sentinel_Tower_Top;
            List.Height        := List.Height + 1;
         end loop;
      end;
      Assert (List.Height >= New_Node_Height);
      -- Create new tower of New_Node_Height items.
      -- Note, first node already created, which is why loop
      -- starts at 2.
      for I in 2 .. New_Node_Height loop
         declare
            Temp : Node_Ptr := New_Node_Top;
         begin
            New_Node_Top :=
              new Node'(Data => Item, Right => null, Below => Temp);
         end;
      end loop;

      -- For each layer, find insertion location, and if layer
      -- is low enough, insert the appropriate node of our tower.
      Insertion :
      declare
         -- Current node in linked list
         Cur_Node       : Node_Ptr := List.Top_Left;
         -- Current level of linked list
         Cur_Level      : Natural  := List.Height;
         -- Current newly created node to insert
         -- into next layer with low enough height.
         Cur_Tower_Node : Node_Ptr := New_Node_Top;
      begin
         while Cur_Node /= null loop
            if Cur_Node.Right = null or else Item < Cur_Node.Right.Data then
               -- Found location where Item should be in this layer
               if Cur_Level <= New_Node_Height then
                  -- Insert node of tower into this list.
                  declare
                     Temp : Node_Ptr := Cur_Node.Right;
                  begin
                     Cur_Node.Right       := Cur_Tower_Node;
                     Cur_Tower_Node.Right := Temp;
                  end;
                  -- Advance to next tower node
                  Cur_Tower_Node := Cur_Tower_Node.Below;
               end if;
               -- Go down to next layer
               Cur_Node  := Cur_Node.Below;
               Cur_Level := Cur_Level - 1;
            else
               -- Advance node to right
               Cur_Node := Cur_Node.Right;
            end if;
         end loop;
         Assert (Cur_Tower_Node = null, "Expected Cur_Tower_Node = null");
      end Insertion;
      List.Size := List.Size + 1;
   end Insert;

   procedure tryRemoveLayer (List : in out Skip_List) is
      currentNode  : Node_Ptr := List.Top_Left;
      removedLayer : Boolean  := False;
      temp         : Node_Ptr;
   begin

      if List.Height = 1 then
         if currentNode.Right = null then
            Free_Node (currentNode);
            List.Height := List.Height - 1;
            return;
         end if;
      end if;

      while currentNode /= null and then not removedLayer loop
         if currentNode.Below /= null and then currentNode.Below.Right = null
         then
            temp := currentNode.Below;

            if currentNode.Below.Below /= null then
               currentNode.Below := currentNode.Below.Below;
            else
               currentNode.Below := null;
            end if;

            removedLayer := True;
            Free_Node (temp);
            List.Height := List.Height - 1;
         end if;

         currentNode := currentNode.Below;
      end loop;
   end tryRemoveLayer;

   -- if(!contains(Item))
   --   return false;
   -- while(current.right <= item)
   --   if(current.right > item)
   --     current = current.below
   --   else
   --     current = current.right
   -- temp = current.right
   -- current.right = current.right.right
   -- free(temp)
   function Remove (List : in out Skip_List; Item : Integer) return Boolean is
      currentNode : Node_Ptr;
      temp        : Node_Ptr;
   begin
      if (List.Size = 0) then
         return False;
      end if;

      if (Contains (List => List, Item => Item) = False) then
         return False;
      end if;

      currentNode := List.Top_Left;
      while currentNode /= null loop
         while currentNode.Right /= null and then currentNode.Right.Data < Item
         loop
            currentNode := currentNode.Right;
         end loop;

         if currentNode.Right /= null and then currentNode.Right.Data = Item
         then

            temp := currentNode.Right;

            if (currentNode.Right.Right /= null) then
               currentNode.Right := currentNode.Right.Right;
            else
               currentNode.Right := null;
            end if;

            Free_Node (temp);

            List.Size := List.Size - 1;

            tryRemoveLayer (List);

            return True;
         end if;

         currentNode := currentNode.Below;
      end loop;

      --should never get here
      return False;
   end Remove;

   -- currentNode = begin
   -- while(item > currentNode.data)
   --   if(currentNode.right is not null)
   --       currentNode = currentNode.right
   --   else
   --     currentNode = currentNode.below
   --   if(currentNode.right is null and currentNode.below is null and currentNode.data != item)
   --     return false
   --   else
   --     return true;
   function Contains (List : Skip_List; Item : Integer) return Boolean is
      currentNode : Node_Ptr := List.Top_Left;
   begin
      if (Size (List => List) = 0) then
         return False;
      end if;

      while currentNode.Data <= Item loop
         if (currentNode.Data = Item) then
            return True;
         else
            if
              (currentNode.Right /= null
               and then currentNode.Right.Data <= Item)
            then
               currentNode := currentNode.Right;
            elsif (currentNode.Below /= null) then
               currentNode := currentNode.Below;
            else
               --should be impossible to get to, but just to be safe`
               return False;
            end if;
         end if;
      end loop;
      return False;
   end Contains;

   function Is_Empty (List : Skip_List) return Boolean is
   begin
      return List.Size = 0;
   end Is_Empty;

   function Size (List : Skip_List) return Natural is
   begin
      return List.Size;
   end Size;

   -- Return the number of layers in the skiplist
   -- An empty skip list has height 0.
   function Height (List : Skip_List) return Natural is
   begin
      return List.Height;
   end Height;
end SkipListPkg;
