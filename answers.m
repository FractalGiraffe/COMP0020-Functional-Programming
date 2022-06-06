>|| Answer Sheet

Question 2.

Each block has:

- Clack's boundary tag

- Data area

Each free block holds in its data area a pointer to the header of the next free block in the free list (unless it is the last free block on the free list) – this means that there is a minimum size for the data area that is just big enough to hold a pointer

Assume there is a maximum limit to the block size that can be requested, so we know how many bits are needed in the block header to represent that size. Sizes may be counted in terms of bytes or words – it is most convenient if the thing being counted is the same as what is meant by the argument n to malloc – assume we are counting bytes

Assume a block header is:

 -  mark flag of this block
 -  live flag and size of previous block
 -  live flag and size of this block

> header_size = 4

A pointer is typically 4 bytes. Since here 0 is a valid memory address, so we can use -1 as the NULL pointer

> min_block_size = 4


> header ::= Header bool (bool, num) (bool, num)
> children ::= [num] || set of pointers contained in data area
> block ::= Block header children
> free_ptr ::= num
> heap ::= Heap [block] free_ptr


> replace :: num -> block -> heap -> heap
> replace p block (Heap blocks free_ptr) = error "seg fault", if p < 0 \/ p >= #blocks
>                                        = Heap (replace_with_block p blocks) free_ptr, otherwise
>                                          where
>                                          replace_with_block 0 (x:xs)       = block:xs
>                                          replace_with_block (p+1) (x:xs)   = x:(replace_with_block p block xs)


> get_block_size :: num -> heap -> num
> get_block_size p (Heap blocks free_ptr) = size
>                                           where
>                                           (Block (Header marked prev_block_info (live, size)) children) = blocks ! p

> get_prev_block_size :: num -> heap -> num
> get_block_size p (Heap blocks free_ptr) = size
>                                           where
>                                           (Block (Header marked (live, size) block_info) children) = blocks ! p

> set_block_size :: num -> num -> heap -> heap
> set_block_size p new_size (Heap blocks free_ptr) = replace p (Block (Header marked prev_block_info (live, new_size)) children) (Heap blocks free_ptr)
>                                                    where
>                                                    (Block (Header marked prev_block_info (live, old_size)) children) = blocks ! p 

> set_prev_block_size :: num -> num -> heap -> heap
> set_prev_block_size p new_size (Heap blocks free_ptr) = replace p (Block (Header marked (live, new_size) block_info) children) (Heap blocks free_ptr)
>                                                         where
>                                                         (Block (Header marked (live, old_size) block_info) children) = blocks ! p

> get_next_free_block :: num -> heap -> num
> get_next_free_block p (Heap blocks free_ptr) = next_free_block
>                                                where
>                                                (Block header [next_free_block]) = blocks ! p

> set_next_free_block :: num -> num -> heap -> heap
> set_next_free_block p next_free_block (Heap blocks free_ptr) = replace p (Block header [next_free_block]) (Heap blocks free_ptr)
>                                                                where
>                                                                (Block header children) = blocks ! p

> is_live :: num -> heap -> bool
> is_live block (Heap blocks free_ptr) = live
>                                        where
>                                        (Block (Header marked prev_block_info (live, size)) children) = blocks ! block

> set_liveness :: num -> bool -> heap -> heap
> set_liveness block live (Heap blocks free_ptr) = replace block (Block (Header marked prev_block_info (live, size)) children) (Heap blocks free_ptr)
>                                                  where
>                                                  (Block (Header marked prev_block_info (was_live, size)) children) = blocks ! block

> first_fit :: num -> heap -> (num, num)
> first_fit n (Heap blocks -1)        = error "out of memory"
> first_fit n (Heap blocks free_ptr)  = (free_ptr, free_block_size), if n <= free_block_size
>                                     = first_fit n (Heap blocks next_free), otherwise
>                                       where
>                                       free_block_size = get_block_size free_ptr (Heap blocks free_ptr)
>                                       next_free = get_next_free_block free_ptr (Heap blocks free_ptr)

> insert_into_free_list :: num -> heap -> heap
> insert_into_free_list block (Heap blocks free_ptr) = set_next_free_block block free_ptr (Heap blocks block), if block < free_ptr
>                                                    = restructure_free_list free_ptr, otherwise
>                                                      where
>                                                      restructure_free_list free_block = set_next_free_block free_block block new_heap, if block < next_free_block
>                                                                                   = restructure_free_list next_free_block, otherwise
>                                                                                     where
>                                                                                     next_free_block = get_next_free_block free_block (Heap blocks free_ptr)
>                                                                                     new_heap  = set_next_free_block block next_free_block (Heap blocks free_ptr)


> remove_from_free_list :: num -> heap -> heap
> remove_from_free_list block (Heap blocks free_ptr) = (Heap blocks (get_next_free_block block (Heap blocks free_ptr))), if block = free_ptr
>                                                    = restructure_free_list free_ptr, otherwise
>                                                      where
>                                                      restructure_free_list free_block = set_next_free_block free_block free_block_after_next (Heap blocks free_ptr), if next_free_block = block
>                                                                                       = restructure_free_list next_free_block, otherwise
>                                                                                         where
>                                                                                         next_free_block = get_next_free_block free_block (Heap blocks free_ptr)                                                                                        
>                                                                                         free_block_after_next = get_next_free_block block (Heap blocks free_ptr)


> sync :: num -> heap -> heap
> sync block my_heap = set_block_size next_block size (set_liveness next_block live my_heap) 
>                      where
>                      size = get_block_size block my_heap
>                      live = is_live block my_heap
>                      next_block = block + 1 

> split :: num -> num -> num -> heap -> heap
> split block n k (Heap blocks free_ptr) = set_block_size block n heap_with_part_synchronized_new_block 
>                                          where
>                                          new_block = Block (Header False (False, n) (False, k - header_size)) []
>                                          ptr_to_new_block = block + 1
>
>                                          insert_new_block_into_heap 0 remaining_blocks = new_block:remaining_blocks
>                                          insert_new_block_into_heap (p+1) block:blocks = block:(insert_new_block_into_heap p blocks)  
> 
>                                          heap_with_new_block = (Heap (insert_new_block_into_heap ptr_to_new_block blocks) free_ptr)
>                                          heap_with_part_synchronized_new_block = sync ptr_to_new_block heap_with_new_block

Malloc takes a variable block size in bytes and a heap and returns a two-tuple of the new heap after allocation and the returned pointer into the new heap using the First-fit allocation policy as it's easiest to code:

> malloc :: num -> heap -> (heap, num)
> malloc n my_heap = (synchronize_allocated_block, allocated_block)
>                    where
>                    (allocated_block, y)  = first_fit (max [min_block_size, n]) my_heap
>                    restructured_heap     = remove_from_free_list allocated_block my_heap, if k < header_size + min_block_size  
>                                          = split_allocated_block_and_restructure_heap, otherwise
>                                            where
>                                            k = y - n
>                                            new_block = allocated_block + 1 
>                                            split_allocated_block = split allocated_block n k my_heap
>                                            insert_new_block_into_free_list = insert_into_free_list new_block split_allocated_block
>                                            split_allocated_block_and_restructure_heap = remove_from_free_list allocated_block insert_new_block_into_free_list  
>                    set_allocated_block_to_live = set_liveness allocated_block True restructured_heap
>                    synchronize_allocated_block = sync allocated_block set_allocated_block_to_live


> remove_from_heap :: num -> heap -> heap
> remove_from_heap block (Heap blocks free_ptr) = (Heap (remove block blocks) free_ptr)
>                                                 where
>                                                 remove 0 remaining_blocks = remaining_blocks
>                                                 remove (p+1) block:blocks = block:(remove p blocks)  


> merge_with_prev :: num -> heap -> (heap, num)
> merge_with_prev block my_heap = (my_heap, block), if is_live prev_block my_heap
>                               = (synchronize_prev_block, prev_block), otherwise  
>                                 where
>                                 block_size = get_block_size block my_heap
>                                 prev_block_size = get_prev_block_size block my_heap
>                                 prev_block = block - 1
>                                 resize_prev_block = set_block_size prev_block (prev_block_size + header_size + block_size) my_heap 
>                                 restructure_heap = remove_from_heap block resize_prev_block
>                                 synchronize_prev_block = sync prev_block restructure_heap 


> merge_with_next :: num -> heap -> heap
> merge_with_next block my_heap = my_heap, if is_live next_block my_heap 
>                               = synchronize_block, otherwise
>                                 where
>                                 block_size = get_block_size block my_heap
>                                 next_block = block + 1
>                                 next_block_size = get_block_size next_block my_heap 
>                                 resize_block = set_block_size block (block_size + header_size + next_block_size) my_heap
>                                 restructure_free_list = remove_from_free_list next_block resize_block 
>                                 restructure_heap = remove_from_heap next_block restructure_free_list 
>                                 synchronize_block = sync block restructure_heap 


Free frees memory according to an AO ordering policy, so the free block with the lowest address is considered first for allocation, which clusters live blocks at lower memory addresses thereby improving virtual memory performance

> free :: num -> heap -> heap
> free block my_heap = my_heap, if block = -1
>                    = synchronize_block, otherwise
>                      where
>                      (heap_after_merge_with_prev, new_block)  = merge_with_prev block my_heap
>                      heap_after_merge_with_next = merge_with_next new_block heap_after_merge_with_prev  
>             
>                      restructure_heap = heap_after_merge_with_next, if block ~= new_block
>                                       = insert_into_free_list block heap_after_merge_with_next, otherwise
>
>                      set_freed_block_to_free = set_liveness new_block False restructure_heap
>                      synchronize_block = sync new_block set_freed_block_to_free


Provide code to simulate the operation of a mark-scan garbage collector:

> header ::= Header num num (bool, num) (bool, num) || i-field n-field

> xmark heap -> num -> heap
> xmark my_heap p = my_heap, if marked 
>                 = foldl xmark new_heap children, otherwise 
>                   where
>                   (Block (Header marked live size) children) = my_heap ! p
>                   marked_block = Block (Header ~marked live size) children
>                   new_heap = replace p marked_block my_heap



> mark :: [num] -> heap -> heap
> mark root_set my_heap = foldl xmark my_heap root_set


> xscan :: heap -> num -> heap 
> xscan (Heap blocks free_ptr) p = (Heap blocks free_ptr), if p >= #blocks
>                                = xscan new_heap_1 (p+1), if marked
>                                = xscan new_heap_2 (p+1), otherwise 
>                                  where
>                                  (Block (Header marked live size) children) = blocks ! p
>                                  unmarked_block = Block (Header ~marked live size) children 
>                                  new_heap_1 = replace p unmarked_block (Heap blocks free_ptr)
>                                  new_heap_2 = free p (Heap blocks free_ptr)

> scan :: heap -> heap
> scan (Heap blocks free_ptr) = xscan (Heap blocks -1) 0


> gc_malloc ::

When a program starts running all of the free memory in the heap will exist in a single contiguous block       
