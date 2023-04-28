with Ada.Unchecked_Deallocation;

-- While it is possible to make the skip list hold any type of item,
-- the project is complicated enough as is,
-- so this skip list will only contain integers.
-- The integers should be arranged in sorted order
-- When a node is inserted, it's height is provided to you.
-- A node appears on level i if and only if it's height is >= i.
-- Level 1 is the bottom level (which contains all the elements)
-- The skip list may contain duplicates. The order in which you
-- insert duplicate values doesn't matter, but make sure to 
-- make consistent choices.
package SkipListPkg is
   
   type Skip_List is limited private;
     
   -- Deallocates all memory asssociated with this skip list,
   -- destroying it permanently.
   procedure Destroy(List : in out Skip_List);
   
   -- This is implemented for you.
   -- It is used as the default if no height argument is 
   -- supplied to the insert procedure.
   function Random_Height return Positive;
   
   -- Insert the Item into the List
   procedure Insert(List : in out Skip_List; 
                    Item : Integer; 
                    New_Node_Height : Positive := Random_Height);
   
   -- Remove the Item from the List.
   -- Return true if item was found (and removed successfully)
   -- and returns false otherwise.
   function Remove(List : in out Skip_List; Item : Integer) return Boolean;
   
   -- Returns True if T is in the list and false otherwise.
   function Contains(List : Skip_List; Item : Integer) return Boolean;
   
   -- Returns True if List is empty and false otherwise.
   function Is_Empty(List : Skip_List) return Boolean;
   
   -- Return the number of layers in the skiplist
   -- An empty skip list has height 0.
   function Height(List : Skip_List) return Natural;
   
   -- Returns the number of elements in List.
   function Size(List : Skip_List) return Natural;
   
   -- Outputs the contents of the Skip_List (on the bottom layer), 
   -- where each item is followed by a single space.
   procedure Put(List : Skip_List);
   
   -- Outputs a more detailed view of the skip list. 
   -- For each layer, outputs the value of each node in the
   -- layer, followed by a space.
   -- Each layer's output is followed by a single new line.
   procedure Detailed_Put(List : Skip_List);
private
   type Node;
   type Node_Ptr is access Node;
   
   type Node is record
      -- The item stored in this node. 
      Data : Integer;
      -- Pointers to the node to the right (in same level)
      -- and node below (one level down).
      Right, Below : Node_Ptr := null;
   end record;
   
   -- A pointer to the top left node of the skip list.
   type Skip_List is record
      Top_Left : Node_Ptr := null;
      Size : Natural := 0;
      Height : Natural := 0;
   end record;

   -- Deallocates the memory associated with a single node.
   procedure Free_Node is new Ada.Unchecked_Deallocation
     (Object => Node,
      Name   => Node_Ptr);
end SkipListPkg;
