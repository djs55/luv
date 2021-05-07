open Test_helpers

let tests = [
  "env", [
    "environ", `Quick, begin fun () ->
      Unix.putenv "LUV_TESTER" "42";
      let environment = Luv.Env.environ () |> check_success_result "environ" in
      Unix.putenv "LUV_TESTER" "";
      let found =
        List.exists
          (fun (name, value) -> name = "LUV_TESTER" && value = "42") environment
      in
      if not found then
        Alcotest.failf "environment variable not found"
    end;
  ];

  "system name", [
    "uname", `Quick, begin fun () ->
      let uname = Luv.System_info.uname () |> check_success_result "uname" in
      let names = ["Linux"; "Darwin"; "MINGW32_NT-10.0"] in
      if not @@ List.mem uname.sysname names then
        Alcotest.failf "sysname: got %s" uname.sysname;
      let machines = ["x86_64"; "arm64"] in
      if not @@ List.mem uname.machine machines then
        Alcotest.failf "machine: got %s" uname.machine;
    end;
  ];

  "time", [
    "gettimeofday", `Quick, begin fun () ->
      let timeval =
        Luv.Time.gettimeofday () |> check_success_result "gettimeofday" in
      let uv_time =
        let open Luv.Time in
        (Int64.to_float timeval.tv_sec) +.
          (Int32.to_float timeval.tv_usec) *. 1e-6
      in
      let ocaml_time = Unix.gettimeofday () in

      let delta = abs_float (uv_time -. ocaml_time) in
      if delta > 1. then
        Alcotest.failf "times: %f %f" uv_time ocaml_time
    end;

    "sleep: basic", `Quick, begin fun () ->
      Luv.Time.sleep 100
    end;

    "sleep: multithreading", `Quick, begin fun () ->
      let start_time = Unix.gettimeofday () in

      let thread =
        Luv.Thread.create (fun () -> Luv.Time.sleep 500)
        |> check_success_result "create"
      in

      Luv.Time.sleep 500;

      Luv.Thread.join thread
      |> check_success_result "join";

      let elapsed = Unix.gettimeofday () -. start_time in
      if elapsed > 0.75 then
        Alcotest.failf "%f s elapsed" elapsed
    end;
  ];

  "random", [
    "async", `Quick, begin fun () ->
      let content = String.make 16 'a' in
      let buffer = Luv.Buffer.from_string content in
      Luv.Random.random buffer begin fun result ->
        check_success_result "random" result;
        if Luv.Buffer.to_string buffer = content then
          Alcotest.fail "buffer contents"
      end;
      run ()
    end;

    "sync", `Quick, begin fun () ->
      let content = String.make 16 'a' in
      let buffer = Luv.Buffer.from_string content in
      Luv.Random.Sync.random buffer
      |> check_success_result "random";
      if Luv.Buffer.to_string buffer = content then
        Alcotest.fail "buffer contents"
    end;
  ];
]
