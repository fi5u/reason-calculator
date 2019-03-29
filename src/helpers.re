// https://stackoverflow.com/a/53208064/997596
let slice = (list, start, range) => {
  let rec drop = (n, list) =>
    switch (list) {
    | [] => []
    | [_, ...xs] as z => n == 0 ? z : drop(n - 1, xs)
    };

  let rec take = (n, list) =>
    switch (list) {
    | [] => []
    | [x, ...xs] => n == 0 ? [] : [x, ...take(n - 1, xs)]
    };

  take(range, drop(start, list));
};