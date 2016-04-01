var h = require('virtual-dom').h;

function h_wrapper(sel, props, children)
{
    var props_ = {};
    for (var i = 0; i < props.length; ++i)
    {
        props_[props[i].slot1] = props[i].slot2;
    }

    var children_ = children.map(function (v, i, arr) { return v.slot1 });

    return h(sel, props_, children_);
}
