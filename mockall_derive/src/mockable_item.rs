// vim: tw=80
use super::*;

pub(crate) enum MockableItem {
    Item(Item),
    ManualMock(ManualMock)
}

impl From<Item> for MockableItem {
    fn from(item: Item) -> MockableItem {
        MockableItem::Item(item)
    }
}

impl From<ManualMock> for MockableItem {
    fn from(mock: ManualMock) -> MockableItem {
        MockableItem::ManualMock(mock)
    }
}
