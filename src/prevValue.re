let component = ReasonReact.statelessComponent("PrevValue");

let make = (~math, ~value, _) => {
  ...component,

  render: _self =>
    <tr className="PrevValue">
      <td className="PrevValue-math">
        {switch (math) {
         | None => ReasonReact.null
         | Some(math) => ReasonReact.string(String.make(1, math))
         }}
      </td>
      <td className="PrevValue-value"> {ReasonReact.string(value)} </td>
    </tr>,
};