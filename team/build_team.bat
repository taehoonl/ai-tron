if "%1" == "" (
  echo usage: %0 ^<team-name^>
) else (
  ocamlc -o %1.exe -I +threads -I ../game -I ../shared unix.cma threads.cma str.cma ../shared/thread_pool.mli ../shared/thread_pool.ml ../shared/connection.mli ../shared/connection.ml ../shared/constants.ml ../shared/definitions.ml ../shared/util.ml ../shared/a_star.mli ../shared/a_star.ml ../shared/state.mli ../shared/state.ml team.ml %1.ml
)
