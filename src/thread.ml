module Request_ =
struct
  type t = [ `Work ] Request.t

  let make () =
    Request.allocate
      ~reference_count:C.Types.Work.reference_count C.Types.Work.t
end

let work_trampoline =
  C.Functions.Work.get_work_trampoline ()

let after_work_trampoline =
  C.Functions.Work.get_after_work_trampoline ()

let queue_work ?loop ?(request = Request_.make ()) f callback =
  let return_value_cell = ref None in
  let f () = return_value_cell := Some (f ()) in
  Request.set_reference ~index:C.Types.Work.function_index request f;
  Request.set_reference ~index:C.Types.Handle.generic_callback_index request
      begin fun result ->

    Request.release request;
    result
    |> Error.to_result_lazy (fun () ->
      match !return_value_cell with
      | Some result -> result
      | None -> assert false)
    |> callback
  end;

  let immediate_result =
    C.Functions.Work.queue
      (Loop.or_default loop) request work_trampoline after_work_trampoline
  in

  if immediate_result < Error.success then begin
    Request.release request;
    callback (Result.Error immediate_result)
  end

let c_work_trampoline =
  C.Functions.Work.get_c_work_trampoline ()

let after_c_work_trampoline =
  C.Functions.Work.get_after_c_work_trampoline ()

let queue_c_work
    ?loop
    ?(request = Request_.make ())
    ?(argument = Nativeint.zero)
    f
    callback =

  Request.set_reference ~index:C.Types.Handle.generic_callback_index request
      begin fun result ->

    Request.release request;
    callback result
  end;

  let result =
    C.Functions.Work.add_c_function_and_argument request f argument in
  if not result then begin
    Request.release request;
    callback Error.enomem
  end

  else begin
    let immediate_result =
      C.Functions.Work.queue
        (Loop.or_default loop) request c_work_trampoline after_c_work_trampoline
    in

    if immediate_result < Error.success then begin
      Request.release request;
      callback immediate_result
    end
  end

module Request = Request_

let set_thread_pool_size ?(if_not_already_set = false) thread_count =
  let already_set =
    try ignore (Unix.getenv "UV_THREADPOOL_SIZE"); true
    with Not_found -> false
  in
  if already_set && if_not_already_set then
    ()
  else
    Unix.putenv "UV_THREADPOOL_SIZE" (string_of_int thread_count)

type 'a t = {
  id : C.Types.Thread.t Ctypes.ptr;
  mutable return_value_cell : 'a option;
}

let self () = {
  id = Ctypes.addr (C.Functions.Thread.self ());
  return_value_cell = Some ()
}

let equal thread_1 thread_2 =
  C.Functions.Thread.equal thread_1.id thread_2.id

let thread_trampoline =
  C.Functions.Thread.get_trampoline ()

let create f =
  let thread = {
    id = Ctypes.addr (Ctypes.make C.Types.Thread.t);
    return_value_cell = None;
  }
  in
  let f () = thread.return_value_cell <- Some (f ()) in
  let f_gc_root = Ctypes.Root.create f in
  let result =
    C.Functions.Thread.create thread.id thread_trampoline f_gc_root in
  if result < Error.success then begin
    Ctypes.Root.release f_gc_root;
    Result.Error result
  end
  else
    Result.Ok thread

let create_c ?(argument = Nativeint.zero) f =
  let thread = {
    id = Ctypes.addr (Ctypes.make C.Types.Thread.t);
    return_value_cell = Some ();
  }
  in
  C.Functions.Thread.create_c thread.id f argument
  |> Error.to_result thread

let join thread =
  C.Blocking.Thread.join thread.id
  |> Error.to_result_lazy (fun () ->
    match thread.return_value_cell with
    | Some value -> value
    | None -> assert false)

module TLS =
struct
  type t = C.Types.TLS.t Ctypes.ptr

  let create () =
    let key = Ctypes.addr (Ctypes.make C.Types.TLS.t) in
    C.Functions.TLS.create key
    |> Error.to_result key

  let delete =
    C.Functions.TLS.delete

  let get key =
    Ctypes.raw_address_of_ptr (C.Functions.TLS.get key)

  let set key value =
    C.Functions.TLS.set key (Ctypes.ptr_of_raw_address value)
end

module Once =
struct
  type t = C.Types.Once.t Ctypes.ptr

  let init () =
    let guard = Ctypes.addr (Ctypes.make C.Types.Once.t) in
    C.Functions.Once.init guard
    |> Error.to_result guard

  let trampoline =
    C.Functions.Once.get_trampoline ()

  external set_callback : (unit -> unit) -> unit = "luv_set_once_callback"

  let once guard f =
    set_callback f;
    C.Functions.Once.once guard trampoline

  let once_c guard f =
    C.Functions.Once.once guard (Ctypes.funptr_of_raw_address f)
end

module Mutex =
struct
  type t = C.Types.Mutex.t Ctypes.ptr

  let init ?(recursive = false) () =
    let mutex = Ctypes.addr (Ctypes.make C.Types.Mutex.t) in
    let result =
      if recursive then
        C.Functions.Mutex.init_recursive mutex
      else
        C.Functions.Mutex.init mutex
    in
    Error.to_result mutex result

  let destroy =
    C.Functions.Mutex.destroy

  let lock =
    C.Blocking.Mutex.lock

  let trylock =
    C.Functions.Mutex.trylock

  let unlock =
    C.Functions.Mutex.unlock
end

module Rwlock =
struct
  type t = C.Types.Rwlock.t Ctypes.ptr

  let init () =
    let rwlock = Ctypes.addr (Ctypes.make C.Types.Rwlock.t) in
    C.Functions.Rwlock.init rwlock
    |> Error.to_result rwlock

  let destroy =
    C.Functions.Rwlock.destroy

  let rdlock =
    C.Blocking.Rwlock.rdlock

  let tryrdlock =
    C.Functions.Rwlock.tryrdlock

  let rdunlock =
    C.Functions.Rwlock.rdunlock

  let wrlock =
    C.Blocking.Rwlock.wrlock

  let trywrlock =
    C.Functions.Rwlock.trywrlock

  let wrunlock =
    C.Functions.Rwlock.wrunlock
end

module Semaphore =
struct
  type t = C.Types.Semaphore.t Ctypes.ptr

  let init count =
    let semaphore = Ctypes.addr (Ctypes.make C.Types.Semaphore.t) in
    C.Functions.Semaphore.init semaphore (Unsigned.UInt.of_int count)
    |> Error.to_result semaphore

  let destroy =
    C.Functions.Semaphore.destroy

  let post =
    C.Functions.Semaphore.post

  let wait =
    C.Blocking.Semaphore.wait

  let trywait =
    C.Functions.Semaphore.trywait
end

module Condition =
struct
  type t = C.Types.Condition.t Ctypes.ptr

  let init () =
    let condition = Ctypes.addr (Ctypes.make C.Types.Condition.t) in
    C.Functions.Condition.init condition
    |> Error.to_result condition

  let destroy =
    C.Functions.Condition.destroy

  let signal =
    C.Functions.Condition.signal

  let broadcast =
    C.Functions.Condition.broadcast

  let wait =
    C.Blocking.Condition.wait

  let timedwait condition mutex interval =
    C.Blocking.Condition.timedwait
      condition mutex (Unsigned.UInt64.of_int interval)
end

module Barrier =
struct
  type t = C.Types.Barrier.t Ctypes.ptr

  let init count =
    let barrier = Ctypes.addr (Ctypes.make C.Types.Barrier.t) in
    C.Functions.Barrier.init barrier (Unsigned.UInt.of_int count)
    |> Error.to_result barrier

  let destroy =
    C.Functions.Barrier.destroy

  let wait =
    C.Blocking.Barrier.wait
end
