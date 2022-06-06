>|| Answer Sheet

Question 2.

Each block has:

- Clack's boundary tag

- Data area

Each free block holds in its data area a pointer to the header of the next free block in the free list (unless it is the last free block on the free list) – this means that there is a minimum size for the data area that is just big enough to hold a pointer

Assume there is a maximum limit to the block size that can be requested, so we know how many bits are needed in the block header to represent that size. Sizes may be counted in terms of bytes or words – it is most convenient if the thing being counted is the same as what is meant by the argument n to malloc – assume we are counting bytes

4 bytes for i-field and n-field, and 4 bytes for Clack's boundary tag

Assume a block header is:

 -  i-field and n-field
 -  live flag and size of previous block
 -  live flag and size of this block

> header_size = 8

A pointer is typically 4 bytes. Since here 0 is a valid memory address, so we can use -1 as the NULL pointer

> min_block_size = 4


> pointer ::= Pointer num
> i_field ::= I_field num
> n_field ::= N_field num
> live ::= Live bool
> size ::= Size num

> block_info ::= Block_info (live, size)
> clacks_boundary_tag block_info block_info

> header ::= Header i_field n_field clacks_boundary_tag
> children ::= Children [pointer]
> block ::= Block header children

> heap ::= Heap [block] pointer

> replace :: num -> * -> [*] -> [*] 
> replace i x []     = error "index out of range"
> replace 0 x (y:ys) = x:ys
> replace i x (y:ys) = error "index out of range", if i < 0 \/ i >= #(y:ys)
>                    = y:(replace (i-1) x ys), otherwise


> replace_block :: num -> block -> heap -> heap
> replace_block target_block my_block (Heap blocks free_ptr) 
>  = error "seg fault", if target_block < 0 \/ target_block >= #blocks
>  = Heap (replace target_block my_block blocks) free_ptr, otherwise


> get_block_size :: num -> heap -> num
> get_block_size my_block (Heap blocks free_ptr) 
>  = my_size
>    where
>    (Block (Header my_i_field my_n_field prev_block_info (my_live, my_size)) my_children) = blocks ! my_block


> get_prev_block_size :: num -> heap -> num
> get_prev_block_size my_block (Heap blocks free_ptr) 
>  = my_size
>    where
>    (Block (Header my_i_field my_n_field (my_live, my_size) my_block_info) my_children) = blocks ! my_block


> set_block_size :: num -> num -> heap -> heap
> set_block_size my_block new_size (Heap blocks free_ptr) 
>  = replace_block my_block (Block (Header marked prev_block_info (live, new_size)) children) (Heap blocks free_ptr)
>    where
>    (Block (Header my_i_field my_n_field prev_block_info (my_live, my_size)) my_children) = blocks ! my_block 


> get_next_free_block :: num -> heap -> num
> get_next_free_block my_block (Heap blocks free_ptr) 
>  = next_free_block
>  where
>  (Block header [next_free_block]) = blocks ! my_block


> set_next_free_block :: num -> num -> heap -> heap
> set_next_free_block my_block next_free_block (Heap blocks free_ptr) 
>  = replace_block my_block (Block header [next_free_block]) (Heap blocks free_ptr)
>    where
>    (Block header children) = blocks ! my_block


> is_live :: num -> heap -> bool
> is_live my_block (Heap blocks free_ptr) 
>  = my_live
>    where
>    (Block (Header my_i_field my_n_field prev_block_info (my_live, my_size)) my_children) = blocks ! my_block


> set_liveness :: num -> bool -> heap -> heap
> set_liveness my_block my_live (Heap blocks free_ptr) 
>  = replace_block my_block new_block (Heap blocks free_ptr)
>    where
>    (Block (Header my_i_field my_n_field prev_block_info (was_live, my_size)) my_children) = blocks ! my_block
>    new_block = Block (Header my_i_field my_n_field prev_block_info (my_live, my_size)) children


> first_fit :: num -> heap -> (num, num)
> first_fit n (Heap blocks (Pointer (-1)))        
>  = error "out of memory"
> first_fit n (Heap blocks free_ptr)  
>  = (free_ptr, free_block_size), if n <= free_block_size
>  = first_fit n (Heap blocks next_free), otherwise
>    where
>    free_block_size = get_block_size free_ptr (Heap blocks free_ptr)
>    next_free = get_next_free_block free_ptr (Heap blocks free_ptr)


> insert_into_free_list :: num -> heap -> heap
> insert_into_free_list block (Heap blocks free_ptr) 
>  = set_next_free_block block free_ptr (Heap blocks block), if block < free_ptr
>  = restructure_free_list free_ptr, otherwise
>    where
>    restructure_free_list free_block 
>     = set_next_free_block free_block block new_heap, if block < next_free_block
>     = restructure_free_list next_free_block, otherwise
>       where
>       next_free_block = get_next_free_block free_block (Heap blocks free_ptr)
>       new_heap  = set_next_free_block block next_free_block (Heap blocks free_ptr)


> remove_from_free_list :: num -> heap -> heap
> remove_from_free_list block (Heap blocks free_ptr) 
>  = (Heap blocks (get_next_free_block block (Heap blocks free_ptr))), if block = free_ptr
>  = restructure_free_list free_ptr, otherwise
>    where
>    restructure_free_list free_block 
>     = set_next_free_block free_block free_block_after_next (Heap blocks free_ptr), if next_free_block = block
>     = restructure_free_list next_free_block, otherwise
>       where
>       next_free_block = get_next_free_block free_block (Heap blocks free_ptr)                                                   >       free_block_after_next = get_next_free_block block (Heap blocks free_ptr)


> sync :: num -> heap -> heap
> sync block my_heap 
>  = set_block_size next_block size (set_liveness next_block live my_heap) 
>    where
>    size = get_block_size block my_heap
>    live = is_live block my_heap
>    next_block = block + 1 


> split :: num -> num -> num -> heap -> heap
> split block n k (Heap blocks free_ptr) 
>  = set_block_size block n heap_with_part_synchronized_new_block 
>    where
>    new_block = Block (Header False (False, n) (False, k - header_size)) []
>    ptr_to_new_block = block + 1
>
>    insert_new_block_into_heap 0 remaining_blocks = new_block:remaining_blocks
>    insert_new_block_into_heap (p+1) block:blocks = block:(insert_new_block_into_heap p blocks)  
> 
>    heap_with_new_block = (Heap (insert_new_block_into_heap ptr_to_new_block blocks) free_ptr)
>    heap_with_part_synchronized_new_block = sync ptr_to_new_block heap_with_new_block


Malloc takes a variable block size in bytes and a heap and returns a two-tuple of the new heap after allocation and the returned pointer into the new heap using the First-fit allocation policy as it's easiest to code:

> malloc :: num -> heap -> (heap, num)
> malloc n (Heap blocks free_ptr) 
>  = error "out of memory", if free_ptr = -1
>  = (synchronize_allocated_block, allocated_block), otherwise
>    where
>    (allocated_block, y)  
>     = first_fit (max [min_block_size, n]) (Heap blocks free_ptr)
>    restructured_heap
>     = remove_from_free_list allocated_block (Heap blocks free_ptr), if k < header_size + min_block_size  
>     = split_allocated_block_and_restructure_heap, otherwise
>       where
>       k = y - n
>       new_block = allocated_block + 1 
>       split_allocated_block = split allocated_block n k (Heap blocks free_ptr)
>       insert_new_block_into_free_list = insert_into_free_list new_block split_allocated_block
>       split_allocated_block_and_restructure_heap = remove_from_free_list allocated_block insert_new_block_into_free_list  
>    set_allocated_block_to_live = set_liveness allocated_block True restructured_heap
>    synchronize_allocated_block = sync allocated_block set_allocated_block_to_live


> remove_from_heap :: num -> heap -> heap
> remove_from_heap block (Heap blocks free_ptr) 
>  = (Heap (remove block blocks) free_ptr)
>    where
>    remove 0 remaining_blocks = remaining_blocks
>    remove (p+1) block:blocks = block:(remove p blocks)  


> merge_with_prev :: num -> heap -> (heap, num)
> merge_with_prev block my_heap 
>  = (my_heap, block), if is_live prev_block my_heap
>  = (synchronize_prev_block, prev_block), otherwise  
>    where
>    block_size = get_block_size block my_heap
>    prev_block_size = get_prev_block_size block my_heap
>    prev_block = block - 1
>    resize_prev_block = set_block_size prev_block (prev_block_size + header_size + block_size) my_heap 
>    restructure_heap = remove_from_heap block resize_prev_block
>    synchronize_prev_block = sync prev_block restructure_heap 


> merge_with_next :: num -> heap -> heap
> merge_with_next block my_heap 
>  = my_heap, if is_live next_block my_heap 
>  = synchronize_block, otherwise
>    where
>    block_size = get_block_size block my_heap
>    next_block = block + 1
>    next_block_size = get_block_size next_block my_heap 
>    resize_block = set_block_size block (block_size + header_size + next_block_size) my_heap
>    restructure_free_list = remove_from_free_list next_block resize_block 
>    restructure_heap = remove_from_heap next_block restructure_free_list 
>    synchronize_block = sync block restructure_heap 


Free frees memory according to an AO ordering policy, so the free block with the lowest address is considered first for allocation, which clusters live blocks at lower memory addresses thereby improving virtual memory performance

> free :: num -> heap -> heap
> free block my_heap 
>  = my_heap, if block = -1
>  = synchronize_block, otherwise
>    where
>    (heap_after_merge_with_prev, new_block) = merge_with_prev block my_heap
>    heap_after_merge_with_next = merge_with_next new_block heap_after_merge_with_prev  
>             
>    restructure_heap = heap_after_merge_with_next, if block ~= new_block
>                     = insert_into_free_list block heap_after_merge_with_next, otherwise
>
>    set_freed_block_to_free = set_liveness new_block False restructure_heap
>    synchronize_block = sync new_block set_freed_block_to_free


Provide code to simulate the operation of a mark-scan garbage collector:

> get_i :: num -> heap -> num
> get_i block (Heap blocks free_ptr) 
>  = i
>    where
>    (Block (Header i n prev_block_info block_info) children) = blocks ! block


> set_i :: num -> num -> heap -> heap
> set_i block new_i (Heap blocks free_ptr) 
>  = replace_block block (Block (Header new_i n prev_block_info block_info) children) (Heap blocks free_ptr)
>    where
>    (Block (Header old_i n prev_block_info block_info) children) = blocks ! block


> get_n :: num -> heap -> num
> get_n block (Heap blocks free_ptr) 
>  = n
>    where
>    (Block (Header i n prev_block_info block_info) children) = blocks ! block


> get_ith_child :: num -> heap -> num
> get_ith_child block (Heap blocks free_ptr) 
>  = children ! (i - 1)
>    where
>    (Block (Header i n prev_block_info block_info) children) = blocks ! block

> set_ith_child :: num -> num -> heap -> heap
> set_ith_child block value (Heap blocks free_ptr) 
>  = replace_block block replace_child (Heap blocks free_ptr)
>    where
>    (Block (Header i n prev_block_info block_info) children) = blocks ! block
>    replace_child = (Block (Header i n prev_block_info block_info) (replace (i - 1) value children))


> unwind :: num -> num -> heap -> (num, num, heap)
> unwind b f my_heap 
>  = (f, tmp, new_heap)
>    where
>    tmp = get_ith_child f my_heap
>    new_heap = set_ith_child f b my_heap 


> rewind :: num -> num -> heap -> (num, num, heap)
> rewind b f my_heap 
>  = (tmp, b, new_heap)
>    where
>    tmp = get_ith_child b my_heap 
>    new_heap = set_ith_child b f my_heap


> xmark :: num -> heap -> num -> heap
> xmark b f my_heap 
>  = my_heap, if f = -1
>  = xmark new_b new_heap new_f
>    where
>    i = get_i f my_heap
>    n = get_n f my_heap
>    increment_i = set_i f (i+1) my_heap
>    (new_b, new_f, new_heap) = rewind b f increment_i, if i > n
>                             = unwind b f increment_i, otherwise


> mark :: [num] -> heap -> heap
> mark root_set my_heap = foldl (converse (xmark (-1))) my_heap root_set 


> xscan :: heap -> num -> heap 
> xscan (Heap blocks free_ptr) p 
>  = (Heap blocks free_ptr), if p >= #blocks
>  = xscan new_heap_1 (p+1), if i > 0
>  = xscan new_heap_2 (p+1), otherwise 
>    where
>    (Block (Header i n prev_block_info block_info) children) = blocks ! p
>    unmarked_block = Block (Header 0 n prev_block_info block_info) children 
>    new_heap_1 = replace p unmarked_block (Heap blocks free_ptr)
>    new_heap_2 = free p (Heap blocks free_ptr)


> scan :: heap -> heap
> scan (Heap blocks free_ptr) = xscan (Heap blocks -1) 0


> gc_malloc :: num -> heap -> [num] -> (heap, num)
> gc_malloc n (Heap blocks free_ptr) root_set 
>  = malloc n restructure_free_list,  if free_ptr = -1
>  = malloc n (Heap blocks free_ptr), otherwise
>    where
>    mark_heap = mark root_set (Heap blocks free_ptr)
>    restructure_free_list = scan mark_heap


When a program starts running all of the memory in the heap will exist in a single contiguous free block

> initial_heap = ([(Block (Header 0 1 (True, 0) (False, 1024)) [-1])], 0)
