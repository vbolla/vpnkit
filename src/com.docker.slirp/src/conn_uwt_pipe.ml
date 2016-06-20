open Lwt

let src =
  let src = Logs.Src.create "usernet" ~doc:"Mirage TCP/IP <-> socket proxy" in
  Logs.Src.set_level src (Some Logs.Debug);
  src

module Log = (val Logs.src_log src : Logs.LOG)

type 'a io = 'a Lwt.t

type buffer = Cstruct.t

type error = Unix.error

let error_message = Unix.error_message

type flow = {
  fd: Uwt.Pipe.t;
  read_buffer_size: int;
  mutable closed: bool;
}

let connect fd =
  let read_buffer_size = 1024 in
  let closed = false in
  { fd; read_buffer_size; closed }

let close t =
  match t.closed with
  | false ->
    t.closed <- true;
    ignore(Uwt.Pipe.close t.fd);
    Lwt.return ()
  | true ->
    Lwt.return ()

let read flow =
  if flow.closed then return `Eof
  else
    let buffer = Lwt_bytes.create flow.read_buffer_size in
    Uwt.Pipe.read_ba flow.fd ~buf:buffer
    >>= function
    | 0 ->
      return `Eof
    | n ->
      return (`Ok (Cstruct.(sub (of_bigarray buffer) 0 n)))

let read_into flow buffer =
  if flow.closed then return `Eof
  else
    Lwt.catch
      (fun () ->
        Lwt_cstruct.(complete (fun cstruct -> Uwt.Pipe.read_ba ~pos:cstruct.Cstruct.off ~len:cstruct.Cstruct.len flow.fd ~buf:cstruct.Cstruct.buffer) buffer)
        >>= fun () ->
        return (`Ok ())
      ) (fun _e -> return `Eof)

let write flow buf =
  if flow.closed then return `Eof
  else
    Lwt.catch
      (fun () ->
        Uwt.Pipe.write_ba ~pos:buf.Cstruct.off ~len:buf.Cstruct.len flow.fd ~buf:buf.Cstruct.buffer
        >>= fun () ->
        return (`Ok ())
      ) (function
        | Unix.Unix_error(Unix.EPIPE, _, _) -> return `Eof
        | e -> fail e)

let writev flow bufs =
  let rec loop = function
    | [] -> return (`Ok ())
    | x :: xs ->
      if flow.closed then return `Eof
      else
        Lwt.catch
          (fun () ->
            Uwt.Pipe.write_ba ~pos:x.Cstruct.off ~len:x.Cstruct.len flow.fd ~buf:x.Cstruct.buffer
            >>= fun () ->
            loop xs
          ) (function
            | Unix.Unix_error(Unix.EPIPE, _, _) -> return `Eof
            | e -> fail e) in
  loop bufs

let shutdown_write flow =
  try
    Uwt.Pipe.shutdown flow.fd
  with
  | Unix.Unix_error(Unix.ENOTCONN, _, _) -> Lwt.return ()
  | e ->
    Log.err (fun f -> f "Socket_stack.TCPV4.shutdown_write: caught %s returning Eof" (Printexc.to_string e));
    Lwt.return ()

let shutdown_read _flow =
  Lwt.return ()
