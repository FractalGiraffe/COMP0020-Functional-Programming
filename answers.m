>|| Answer Sheet

Question 2.

Each block has:

- Clack's boundary tag

- Data area

Each free block holds in its data area a pointer to the header of the next free block in the free list (unless it is the last free block on the free list) – this means that there is a minimum size for the data area that is just big enough to hold a pointer

Assume there is a maximum limit to the block size that can be requested, so we know how many bits are needed in the block header to represent that size. Sizes may be counted in terms of bytes or words – it is most convenient if the thing being counted is the same as what is meant by the argument n to malloc – assume we are counting bytes

A 16-bit signed integer can provide availability using the sign bit (positive numbers are live, negative numbers are free) and size (the absolute value of the number). Two 16-bit signed integers can be held in a 4-byte header

Assume a block header is 4 bytes:

 -  2 bytes to represent block size and availability of previous block
 -  2 bytes to represent block size and availability of this block

> header_size = 4

A pointer is a 32 bit signed since here 0 is a valid memory address, so we can use -1 as the NULL pointer

> min_block_size = 4

The heap is modelled as a two-tuple comprising an area of memory, modelled as a list of char (byte), and the value for the Free List Pointer. The lowest memory address is the head of the list and the highest memory address is the tail of the list. 

We use a byte-addressable scheme and big-endian byte order

> heap == ([char], num)

Write writes an arbitrary number of bytes to the heap

> write :: [char] -> num -> heap -> heap 
> write bytes ptr (h, free_ptr) = error "seg fault", if ptr < 0 \/ ptr + #bytes - 1 >= #h  
>                               = ((write_ bytes ptr h), free_ptr), otherwise
>                                 where 
>                                 write_ [] ptr old_heap     = old_heap
>                                 write_ (x:xs) 0 (y:ys)     = x:(write_ xs 0 ys)
>                                 write_ bytes ptr (y:ys)    = y:(write_ bytes (ptr-1) ys)

My_read reads an arbitrary number of bytes from the heap

> my_read :: heap -> num -> num -> [char]
> my_read (h, free_ptr) ptr num_bytes = error "seg fault", if ptr < 0 \/ ptr + num_bytes - 1 >= #h
>                                     = xread h ptr (ptr + num_bytes - 1) [], otherwise
>                                       where
>                                       xread h start current result = result, if current < start
>                                                                    = xread h start (current - 1) (h ! current : result), otherwise

Encode encodes an integer in two's complement with a given bit width 

> encode :: num -> num -> [char]
> encode n width = error "width must be greater than zero", if width = 0
>                = error "data must be naturally aligned", if width mod 8 ~= 0
>                = error "width is too small to represent n", if n > 2^(width-1) - 1 \/ n < -2^(width-1)
>                = xencode (n + 2^width) (width div 8) 0 [], if n < 0
>                = xencode n (width div 8) 0 [], otherwise 
>                  where
>                  xencode n total_bytes byte_number encoded_n = encoded_n, if byte_number = total_bytes
>                                                              = xencode n total_bytes (byte_number + 1) (decode ((n div 2^(8 * byte_number)) mod 2^8) : encoded_n), otherwise

Decode_ decodes a byte string to an integer

> decode_ :: [char] -> num -> num
> decode_ encoded_n width = error "width must be greater than zero", if width = 0
>                         = error "data must be naturally aligned", if width mod 8 ~= 0
>                         = error "promotion is not supported", if #encoded_n < width / 8
>                         = to_signed (xdecode_ encoded_n (width div 8) 0 0), otherwise
>                           where xdecode_ encoded_n total_bytes char_index decoded_n = decoded_n, if char_index = total_bytes
>                                                                                     = xdecode_ encoded_n total_bytes (char_index + 1) (decoded_n + (code (encoded_n ! char_index))*2^(width - 8 - 8*char_index)), otherwise
>
>                                 to_signed n = n - 2^width, if n div 2^(width - 1) = 1
>                                             = n, otherwise

> write_int16 :: num -> num -> heap -> heap
> write_int16 n ptr my_heap = write (encode n 16) ptr my_heap

> read_int16 :: heap -> num -> num
> read_int16 my_heap ptr = decode_ (my_read my_heap ptr 2) 16

> write_int32 :: num -> num -> heap -> heap
> write_int32 n ptr my_heap = write (encode n 32) ptr my_heap

> read_int32 :: heap -> num -> num
> read_int32 my_heap ptr = decode_ (my_read my_heap ptr 4) 32 


> get_block_size :: num -> heap -> num
> get_block_size block my_heap = abs (read_int16 my_heap (block - header_size div 2))

> get_prev_block_size :: num -> heap -> num
> get_prev_block_size block my_heap = get_block_size (block - header_size div 2) my_heap

> set_block_size :: num -> num -> heap -> heap
> set_block_size block block_size my_heap = write_int16 block_size (block - header_size div 2) my_heap 

> set_prev_block_size :: num -> num -> heap -> heap
> set_prev_block_size block prev_block_size my_heap = set_block_size (block - header_size div 2) prev_block_size my_heap 

> get_next_free_block :: num -> heap -> num
> get_next_free_block free_block my_heap = read_int32 my_heap free_block

> set_next_free_block :: num -> num -> heap -> heap
> set_next_free_block free_block next_free_block my_heap = write_int32 next_free_block free_block my_heap

> is_free :: num -> heap -> bool
> is_free block my_heap = block_size < 0
>                         where
>                         block_size = read_int16 my_heap (block - header_size div 2)

> first_fit :: num -> heap -> (num, num)
> first_fit n (h, -1)       = error "out of memory"  
> first_fit n (h, free_ptr) = (free_block, free_block_size), if n <= free_block_size
>                           = first_fit n (h, next_free_block), otherwise
>                             where
>                             free_block = free_ptr + header_size
>                             free_block_size = get_block_size free_block (h, free_ptr)
>                             next_free_block = get_next_free_block free_block (h, free_ptr)


> change_liveness :: num -> heap -> heap
> change_liveness block my_heap = set_block_size block (- block_size) my_heap
>                                 where
>                                 block_size = get_block_size block my_heap                              


> insert_into_free_list :: num -> heap -> heap
> insert_into_free_list block (h, free_ptr) = set_next_free_block block free_ptr (h, block), if block < free_ptr
>                                           = restructure_free_list block (h, free_ptr) free_ptr, otherwise
>                                             where
>                                             restructure_free_list block (h, free_ptr) free_block = set_next_free_block free_block block new_heap, if block < next_free_block
>                                                                                                  = restructure_free_list block (h, free_ptr) next_free_block, otherwise
>                                                                                                    where
>                                                                                                    next_free_block = get_next_free_block free_block (h, free_ptr)
>                                                                                                    new_heap        = set_next_free_block block next_free_block (h, free_ptr)


> remove_from_free_list :: num -> heap -> heap
> remove_from_free_list block (h, free_ptr) = (h, get_next_free_block block (h, free_ptr)), if block = free_ptr
>                                           = restructure_free_list block (h, free_ptr) free_ptr, otherwise
>                                             where
>                                             restructure_free_list block (h, free_ptr) free_block = set_next_free_block free_block free_block_after_next (h, free_ptr), if next_free_block = block
>                                                                                                  =  restructure_free_list block (h, free_ptr) next_free_block, otherwise
>                                                                                                     where
>                                                                                                     next_free_block = get_next_free_block free_block (h, free_ptr)                                                                                        
>                                                                                                     free_block_after_next = get_next_free_block block (h, free_ptr)


> sync :: num -> heap -> heap
> sync block my_heap = set_prev_block_size next_block block_size my_heap
>                      where
>                      block_size = get_block_size block my_heap
>                      next_block = block + block_size + header_size


> split :: num -> num -> num -> heap -> (heap, num)
> split block n k my_heap = (heap_with_synchronized_blocks, new_block) 
>                           where
>                           new_block = block + n + header_size
>                           heap_with_part_new_block_header = set_block_size new_block (-(k - header_size)) my_heap
>                           heap_with_synchronized_new_block = sync new_block heap_with_part_new_block_header
>                           heap_with_updated_block_header = set_block_size block n heap_with_synchronized_new_block
>                           heap_with_synchronized_blocks = sync block heap_with_updated_block_header

Malloc takes a variable block size in bytes and a heap and returns a two-tuple of the new heap after allocation and the returned pointer into the new heap using the First-fit allocation policy as it's easiest to code:

> malloc :: num -> heap -> (heap, num)
> malloc n my_heap = (new_heap, allocated_block)
>                    where
>                    (allocated_block, y)  = first_fit (max [min_block_size, n]) my_heap
>                    restructured_heap     = remove_from_free_list allocated_block my_heap, if k < header_size + min_block_size  
>                                          = split_allocated_block_restructure_heap, otherwise
>                                            where
>                                            k = y - n 
>                                            (heap_after_split, new_block) = split allocated_block n k my_heap
>                                            insert_new_block_into_free_list = insert_into_free_list new_block heap_after_split
>                                            split_allocated_block_restructure_heap = remove_from_free_list allocated_block insert_new_block_into_free_list  
>                    restructured_heap_with_live_allocated_block = change_liveness allocated_block restructured_heap
>                    new_heap = sync allocated_block restructured_heap_with_live_allocated_block


> merge_with_prev :: num -> heap -> (heap, num)
> merge_with_prev block my_heap = (heap_with_synchronized_blocks, prev_block), if is_free prev_block my_heap  
>                               = (my_heap, block), otherwise
>                                 where
>                                 block_size = get_block_size block my_heap
>                                 prev_block_size = get_prev_block_size block my_heap
>                                 prev_block = block - header_size - prev_block_size 
>                                 heap_with_updated_prev_block_header = set_block_size prev_block (prev_block_size + header_size + block_size) my_heap 
>                                 heap_with_synchronized_blocks = sync prev_block heap_with_updated_prev_block_header 


> merge_with_next :: num -> heap -> heap
> merge_with_next block my_heap = heap_with_synchronized_blocks, if is_free next_block my_heap
>                               = my_heap, otherwise
>                                 where
>                                 block_size = get_block_size block my_heap
>                                 next_block = block + block_size + header_size
>                                 next_block_size = get_block_size next_block my_heap 
>                                 heap_with_updated_block_header = set_block_size block (block_size + header_size + next_block_size) my_heap
>                                 heap_with_synchronized_blocks = sync block heap_with_updated_block_header 


Free frees memory according to an AO ordering policy, so the free block with the lowest address is considered first for allocation, which clusters live blocks at lower memory addresses thereby improving virtual memory performance

> free :: num -> heap -> heap
> free block my_heap = my_heap, if block = -1
>                    = change_liveness block restructured_heap, otherwise
>                      where                      
>                      restructured_heap = insert_into_free_list block my_heap
>                       


When a program starts running all of the free memory in the heap will exist in a single contiguous block
