type inputValue = string;
type state = inputValue;

/**
 * Get string value from input event
 */
let getValueFromEvent = (evt): string =>
  (evt |> ReactEvent.Form.target)##value;

let component = ReasonReact.statelessComponent("Input");

let make = (~onChange, ~onSubmit, ~value, _) => {
  ...component,

  render: _self =>
    <input
      value
      type_="text"
      placeholder="Type a calculation"
      onChange={evt => onChange(getValueFromEvent(evt))}
      onKeyDown={evt =>
        if (ReactEvent.Keyboard.key(evt) == "Enter") {
          onSubmit(value);
        }
      }
    />,
};