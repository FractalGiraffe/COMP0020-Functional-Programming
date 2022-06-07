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

Note:

Since we use a block's index in the heap as a 'pointer' for the sake of this simulation, a side effect of this is that a block's index changes as blocks are split and coalesce. However, in terms of a byte-addressable scheme, the given block has the same address.

> min_block_size = 4


> header ::= Header num num (bool, num) (bool, num)
> children ::= Children [num]
> block ::= Block header children
> heap ::= Heap [block] num


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


> set_block_size :: num -> num -> heap -> heap
> set_block_size my_block new_size (Heap blocks free_ptr)
>  = replace_block my_block new_block (Heap blocks free_ptr)
>    where
>    (Block (Header my_i_field my_n_field prev_block_info (my_live, my_size)) my_children) = blocks ! my_block
>    new_block = (Block (Header my_i_field my_n_field prev_block_info (my_live, new_size)) my_children)


> get_prev_block_size :: num -> heap -> num
> get_prev_block_size my_block (Heap blocks free_ptr) 
>  = my_size
>    where
>    (Block (Header my_i_field my_n_field (my_live, my_size) my_block_info) my_children) = blocks ! my_block


> set_prev_block_size :: num -> num -> heap -> heap
> set_prev_block_size my_block new_size (Heap blocks free_ptr)
>  = replace_block my_block new_block (Heap blocks free_ptr) 
>    where
>    (Block (Header my_i_field my_n_field (my_live, my_size) my_block_info) my_children) = blocks ! my_block
>    new_block = (Block (Header my_i_field my_n_field (my_live, new_size) my_block_info) my_children)


> get_next_free_block :: num -> heap -> num
> get_next_free_block my_block (Heap blocks free_ptr) 
>  = next_free_block
>    where
>    (Block header (Children [next_free_block])) = blocks ! my_block


> set_next_free_block :: num -> num -> heap -> heap
> set_next_free_block my_block next_free_block (Heap blocks free_ptr) 
>  = replace_block my_block new_block (Heap blocks free_ptr)
>    where
>    (Block (Header my_i_field my_n_field prev_block_info my_block_info) my_children) = blocks ! my_block
>    new_block = Block (Header my_i_field 1 prev_block_info my_block_info) (Children [next_free_block])

> is_live :: num -> heap -> bool
> is_live my_block (Heap blocks free_ptr)
>  = True, if my_block = -1 
>  = my_live, otherwise
>    where
>    (Block (Header my_i_field my_n_field prev_block_info (my_live, my_size)) my_children) = blocks ! my_block

> is_prev_live :: num -> heap -> bool
> is_prev_live my_block (Heap blocks free_ptr)
>  = my_live
>    where
>    (Block (Header my_i_field my_n_field (my_live, my_size) block_info) my_children) = blocks ! my_block


> set_liveness :: num -> bool -> heap -> heap
> set_liveness my_block my_live (Heap blocks free_ptr) 
>  = replace_block my_block new_block (Heap blocks free_ptr)
>    where
>    (Block (Header my_i_field my_n_field prev_block_info (was_live, my_size)) my_children) = blocks ! my_block
>    new_block = Block (Header my_i_field my_n_field prev_block_info (my_live, my_size)) my_children

> set_prev_liveness :: num -> bool -> heap -> heap
> set_prev_liveness my_block my_live (Heap blocks free_ptr)
>  = replace_block my_block new_block (Heap blocks free_ptr)
>    where
>    (Block (Header my_i_field my_n_field (was_live, my_size) block_info) my_children) = blocks ! my_block
>    new_block = Block (Header my_i_field my_n_field (my_live, my_size) block_info) my_children


> set_n :: num -> num -> heap -> heap
> set_n my_block new_n (Heap blocks free_ptr)
>  = replace_block my_block new_block (Heap blocks free_ptr)
>    where
>    (Block (Header i n prev_block_info block_info) my_children) = blocks ! my_block
>    new_block = Block (Header i new_n prev_block_info block_info) my_children


> clear_children :: num -> heap -> heap
> clear_children my_block (Heap blocks free_ptr)
>  = replace_block my_block new_block (Heap blocks free_ptr)
>    where
>    (Block (Header i n prev_block_info block_info) my_children) = blocks ! my_block
>    new_block = Block (Header i n prev_block_info block_info) (Children [])


> first_fit :: num -> heap -> bool -> (num, num)
> first_fit n (Heap blocks (-1)) fail_silently
>  = (-1, 0), if fail_silently         
>  = error "out of memory", otherwise
> first_fit n (Heap blocks free_ptr) fail_silently 
>  = (free_ptr, free_block_size), if n <= free_block_size
>  = first_fit n (Heap blocks next_free) fail_silently, otherwise
>    where
>    free_block_size = get_block_size free_ptr (Heap blocks free_ptr)
>    next_free = get_next_free_block free_ptr (Heap blocks free_ptr)


> insert_into_free_list :: num -> heap -> heap
> insert_into_free_list my_block (Heap blocks free_ptr)  
>  = set_next_free_block my_block free_ptr (Heap blocks my_block), if free_ptr = -1 \/ my_block < free_ptr
>  = restructure_free_list free_ptr, otherwise
>    where
>    restructure_free_list free_block 
>     = set_next_free_block free_block my_block new_heap, if my_block < next_free_block \/ next_free_block = -1
>     = restructure_free_list next_free_block, otherwise
>       where
>       next_free_block = get_next_free_block free_block (Heap blocks free_ptr)
>       new_heap = set_next_free_block my_block next_free_block (Heap blocks free_ptr)


> remove_from_free_list :: num -> heap -> heap
> remove_from_free_list my_block (Heap blocks free_ptr) 
>  = (Heap blocks (get_next_free_block my_block (Heap blocks free_ptr))), if my_block = free_ptr
>  = restructure_free_list free_ptr, otherwise
>    where
>    restructure_free_list free_block 
>     = set_next_free_block free_block free_block_after_next (Heap blocks free_ptr), if next_free_block = my_block
>     = restructure_free_list next_free_block, otherwise
>       where
>       next_free_block = get_next_free_block free_block (Heap blocks free_ptr)
>       free_block_after_next = get_next_free_block my_block (Heap blocks free_ptr)


> sync :: num -> heap -> heap
> sync my_block (Heap blocks free_ptr) 
>  = (Heap blocks free_ptr), if next_block >=  #blocks
>  = set_prev_block_size next_block my_size (set_prev_liveness next_block my_live (Heap blocks free_ptr)), otherwise 
>    where
>    my_size = get_block_size my_block (Heap blocks free_ptr)
>    my_live = is_live my_block (Heap blocks free_ptr)
>    next_block = my_block + 1 


> insert :: num -> * -> [*] -> [*]
> insert i x [] = [x]
> insert 0 x ys = x:ys
> insert i x (y:ys) = error "index out of range", if i < 0 \/ i > #(y:ys)
>                   = y:(insert (i-1) x ys), otherwise


> insert_into_heap :: num -> block -> heap -> heap
> insert_into_heap ptr_to_new_block new_block (Heap blocks free_ptr)
>  = restructure_heap (Heap new_blocks free_ptr) 0
>    where
>    new_blocks = insert ptr_to_new_block new_block blocks
>    restructure_heap my_heap current_block
>     = my_heap, if current_block >= #new_blocks
>     = restructure_heap (replace_block current_block new_block my_heap) (current_block+1), otherwise
>       where
>       (Block my_header (Children my_children)) = new_blocks ! current_block
>       update child = child + 1, if child > ptr_to_new_block
>                    = child, otherwise
>       new_block = Block my_header (Children (map update my_children))


> split :: num -> num -> num -> heap -> heap
> split my_block n k (Heap blocks free_ptr) 
>  = set_block_size my_block n heap_with_part_synchronized_new_block 
>    where
>    new_block = Block (Header 0 0 (False, n) (False, k - header_size)) (Children [])
>    ptr_to_new_block = my_block + 1
>    heap_with_new_block = insert_into_heap ptr_to_new_block new_block (Heap blocks free_ptr)
>    heap_with_part_synchronized_new_block = sync ptr_to_new_block heap_with_new_block


Malloc takes a variable block size in bytes and a heap and returns a two-tuple of the new heap after allocation and the returned pointer into the new heap:

> malloc :: num -> heap -> bool -> (heap, num)
> malloc 0 (Heap blocks free_ptr) fail_silently
>  = ((Heap blocks free_ptr), (-1))
> malloc n (Heap blocks (-1)) fail_silently
>  = ((Heap blocks (-1)), (-1)), if fail_silently
>  = error "out of memory", otherwise
> malloc n (Heap blocks free_ptr) fail_silently
>  = ((Heap blocks free_ptr), allocated_block), if allocated_block = -1
>  = (synchronize_allocated_block, allocated_block), otherwise
>    where
>    new_n = max [min_block_size, n]
>    (allocated_block, y)  
>     = first_fit new_n (Heap blocks free_ptr) fail_silently
>    restructured_heap
>     = remove_from_free_list allocated_block (Heap blocks free_ptr), if k < header_size + min_block_size  
>     = split_allocated_block_and_restructure_heap, otherwise
>       where
>       k = y - new_n
>       new_block = allocated_block + 1 
>       split_allocated_block = split allocated_block new_n k (Heap blocks free_ptr)
>       insert_new_block_into_free_list = insert_into_free_list new_block split_allocated_block
>       split_allocated_block_and_restructure_heap = remove_from_free_list allocated_block insert_new_block_into_free_list  
>    set_allocated_block_to_live = set_liveness allocated_block True restructured_heap
>    logically_delete_allocated_block_next_free = set_n allocated_block 0 (clear_children allocated_block set_allocated_block_to_live)
>    synchronize_allocated_block = sync allocated_block logically_delete_allocated_block_next_free


> remove :: num -> [*] -> [*]
> remove i []     = error "index out of range"
> remove 0 (y:ys) = ys
> remove i (y:ys) = error "index out of range", if i < 0 \/ i >= #(y:ys)
>                 = y:(remove (i-1) ys), otherwise


> remove_from_heap :: num -> heap -> heap
> remove_from_heap my_block (Heap blocks free_ptr) 
>  = restructure_heap (Heap new_blocks free_ptr) 0
>    where
>    new_blocks = remove my_block blocks
>    restructure_heap my_heap current_block
>     = my_heap, if current_block >= #new_blocks 
>     = restructure_heap (replace_block current_block new_block my_heap) (current_block+1), otherwise
>       where
>       (Block my_header (Children my_children)) = new_blocks ! current_block
>       update child = child - 1, if child > my_block
>                    = child, otherwise
>       new_block = Block my_header (Children (map update my_children))


> merge_with_prev :: num -> heap -> (heap, num)
> merge_with_prev my_block my_heap 
>  = (my_heap, my_block), if is_prev_live my_block my_heap
>  = (synchronize_prev_block, prev_block), otherwise  
>    where
>    block_size = get_block_size my_block my_heap
>    prev_block_size = get_prev_block_size my_block my_heap
>    prev_block = my_block - 1
>    resize_prev_block = set_block_size prev_block (prev_block_size + header_size + block_size) my_heap 
>    restructure_heap = remove_from_heap my_block resize_prev_block
>    synchronize_prev_block = sync prev_block restructure_heap 


> merge_with_next :: num -> heap -> heap
> merge_with_next my_block (Heap blocks free_ptr)  
>  = (Heap blocks free_ptr), if next_block >= #blocks \/ is_live next_block (Heap blocks free_ptr) 
>  = synchronize_block, otherwise
>    where
>    block_size = get_block_size my_block (Heap blocks free_ptr)
>    next_block = my_block + 1
>    next_block_size = get_block_size next_block (Heap blocks free_ptr) 
>    resize_block = set_block_size my_block (block_size + header_size + next_block_size) (Heap blocks free_ptr)
>    restructure_free_list = remove_from_free_list next_block resize_block 
>    restructure_heap = remove_from_heap next_block restructure_free_list 
>    synchronize_block = sync my_block restructure_heap 


Free frees memory according to an AO ordering policy, so the free block with the lowest address is considered first for allocation, which clusters live blocks at lower memory addresses thereby improving virtual memory performance

> free :: num -> heap -> heap
> free my_block (Heap blocks free_ptr) 
>  = (Heap blocks free_ptr), if my_block = -1
>  = error "seg fault", if my_block < 0 \/ my_block >= #blocks
>  = error "double free", if ~(is_live my_block (Heap blocks free_ptr))
>  = synchronize_block, otherwise
>    where
>    (heap_after_merge_with_prev, new_block) = merge_with_prev my_block (Heap blocks free_ptr)
>    heap_after_merge_with_next = merge_with_next new_block heap_after_merge_with_prev  
>             
>    restructure_heap = heap_after_merge_with_next, if my_block ~= new_block
>                     = insert_into_free_list my_block heap_after_merge_with_next, otherwise
>
>    set_freed_block_to_free = set_liveness new_block False restructure_heap
>    synchronize_block = sync new_block set_freed_block_to_free


Provide code to simulate the operation of a mark-scan garbage collector:

> get_i :: num -> heap -> num
> get_i my_block (Heap blocks free_ptr) 
>  = i
>    where
>    (Block (Header i n prev_block_info block_info) my_children) = blocks ! my_block


> set_i :: num -> num -> heap -> heap
> set_i my_block new_i (Heap blocks free_ptr) 
>  = replace_block my_block (Block (Header new_i n prev_block_info block_info) my_children) (Heap blocks free_ptr)
>    where
>    (Block (Header old_i n prev_block_info block_info) my_children) = blocks ! my_block


> get_n :: num -> heap -> num
> get_n my_block (Heap blocks free_ptr) 
>  = n
>    where
>    (Block (Header i n prev_block_info block_info) my_children) = blocks ! my_block


> get_ith_child :: num -> heap -> num
> get_ith_child my_block (Heap blocks free_ptr) 
>  = my_children ! (i - 1)
>    where
>    (Block (Header i n prev_block_info block_info) (Children my_children)) = blocks ! my_block

> set_ith_child :: num -> num -> heap -> heap
> set_ith_child my_block value (Heap blocks free_ptr) 
>  = replace_block my_block replace_child (Heap blocks free_ptr)
>    where
>    (Block (Header i n prev_block_info block_info) (Children my_children)) = blocks ! my_block
>    replace_child = Block (Header i n prev_block_info block_info) (Children (replace (i - 1) value my_children))


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


> xmark :: num -> num -> heap -> heap
> xmark b f my_heap 
>  = my_heap, if b = -1 & i > n                 || root has already been visited
>  = increment_i, if b = -1 & i = n             || finished processing root
>  = xmark prev_b1 prev_f1 prev_heap1, if i > n || node already visited
>  = xmark prev_b2 prev_f2 prev_heap2, if i = n || finished processing node
>  = xmark next_b next_f next_heap, otherwise   || process node's next pointer
>    where
>    i = get_i f my_heap
>    n = get_n f my_heap
>    increment_i = set_i f (i+1) my_heap
>    (prev_b1, prev_f1, prev_heap1) = rewind b f my_heap 
>    (prev_b2, prev_f2, prev_heap2) = rewind b f increment_i
>    (next_b, next_f, next_heap) = unwind b f increment_i


> mark :: [num] -> heap -> heap
> mark root_set my_heap = foldl (converse (xmark (-1))) my_heap root_set 


> xscan :: heap -> num -> heap 
> xscan (Heap blocks free_ptr) my_block 
>  = (Heap blocks free_ptr), if my_block >= #blocks
>  = xscan new_heap_1 (my_block+1), if i > 0
>  = xscan new_heap_2 my_block, if ~is_prev_live 
>  = xscan new_heap_2 (my_block+1), otherwise 
>    where
>    (Block (Header i n (is_prev_live, prev_size) block_info) children) = blocks ! my_block
>    unmarked_block = Block (Header 0 n (is_prev_live, prev_size) block_info) children 
>    new_heap_1 = replace_block my_block unmarked_block (Heap blocks free_ptr)
>    new_heap_2 = free my_block (Heap blocks free_ptr)


> empty_free_list :: heap -> heap
> empty_free_list (Heap blocks (-1)) = (Heap blocks (-1))
> empty_free_list (Heap blocks free_ptr) = empty_free_list (sync free_ptr (set_liveness free_ptr True (remove_from_free_list free_ptr (Heap blocks free_ptr))))   


> my_scan :: heap -> heap
> my_scan (Heap blocks free_ptr) = xscan (empty_free_list (Heap blocks free_ptr)) 0


> gc_malloc :: num -> heap -> [num] -> (heap, num)
> gc_malloc n (Heap blocks free_ptr) root_set 
>  = malloc n restructure_free_list False, if request = ((Heap blocks free_ptr), (-1))
>  = request, otherwise
>    where
>    request = malloc n (Heap blocks free_ptr) True
>    mark_heap = mark root_set (Heap blocks free_ptr)
>    restructure_free_list = my_scan mark_heap


When a program starts running all of the memory in the heap will exist in a single contiguous free block

> initial_heap :: heap
> initial_heap = (Heap [(Block (Header 0 1 (True, 0) (False, 1024)) (Children [-1]))] 0)

