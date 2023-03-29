use super::code::Code;

/// We have two register allocators, both fundamentally derived from Chaitin's Yorktown
/// allocator:
/// http://cs.gmu.edu/~white/CS640/p98-chaitin.pdf
///
/// We have an implementation of Briggs's optimistic allocator which is derivative of Chaitin's allocator:
/// http://www.cs.utexas.edu/users/mckinley/380C/lecs/briggs-thesis-1992.pdf
///
/// And an implementation of Andrew Appel's Iterated Register Coalescing which is derivative of Briggs's allocator.
/// http://www.cs.cmu.edu/afs/cs/academic/class/15745-s07/www/papers/george.pdf
pub fn allocate_registers_by_graph_coloring(_code: &mut Code) {

}