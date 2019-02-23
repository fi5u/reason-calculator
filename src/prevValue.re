let component = ReasonReact.statelessComponent("PrevValue");

let make = (~value, _) => {
  ...component,

  render: _self =>
    <div className="PrevValue"> {ReasonReact.string(value)} </div>,
};