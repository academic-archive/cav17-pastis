module Make_Graph(Q: CS_Interop_Types.CS_Querier) = struct

  include Q

  let blk_foldr f x bd =
    let n = get_nins bd in
    let x = ref x in
    for i = n-1 downto 0 do
      x := f (get_ins bd i) !x;
    done;
    !x

  let blk_foldl f x bd =
    let n = get_nins bd in
    let x = ref x in
    for i = 0 to n-1 do
      x := f (get_ins bd i) !x;
    done;
    !x

  let fun_iteri f fd =
    let n = get_nblk fd in
    for i = 0 to n-1 do
      f i (get_blk fd i);
    done

  (* GOAL: Make a function 'graph' that fetches a function
     using a Querier module Q and turns it into a graph
     representation as in Graph_Types.
     Ignore function calls at first. *)

  let graph _f =
    init ();
    ()

end
