let print_sockaddr = function
  | Error e -> Printf.eprintf "error: %s\n%!" (Luv.Error.strerror e)
  | Ok sa -> Printf.eprintf "to_string = %s, port = %s\n%!"
    (match Luv.Sockaddr.to_string sa with Some x -> x | None -> "None")
    (match Luv.Sockaddr.port sa with Some x -> Printf.sprintf "%d" x | None -> "None")

let () =
  let address = Luv.Sockaddr.ipv4 "127.0.0.1" 7000 |> Result.get_ok in
  let server = Luv.TCP.init () |> Result.get_ok in
  ignore (Luv.TCP.bind server address);

  Printf.eprintf "getsockname server: ";
  Luv.TCP.getsockname server |> print_sockaddr;

  Luv.Stream.listen server begin function
    | Error e ->
      Printf.eprintf "Listen error: %s\n" (Luv.Error.strerror e)
    | Ok () ->
      Printf.eprintf "getsockname server: ";
      Luv.TCP.getsockname server |> print_sockaddr;

      let client = Luv.TCP.init () |> Result.get_ok in

      match Luv.Stream.accept ~server ~client with
      | Error _ ->
        Luv.Handle.close client ignore
      | Ok () ->
        Printf.eprintf "getpeername client: ";
        Luv.TCP.getpeername client |> print_sockaddr;

        Luv.Stream.read_start client begin function
          | Error `EOF ->
            Luv.Handle.close client ignore
          | Error e ->
            Printf.eprintf "Read error: %s\n" (Luv.Error.strerror e);
            Luv.Handle.close client ignore
          | Ok buffer ->
            Luv.Stream.write client [buffer] (fun _ -> ignore)
        end
  end;

  ignore (Luv.Loop.run () : bool)
