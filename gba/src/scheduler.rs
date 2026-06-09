use std::cmp::Ordering;
use std::collections::BinaryHeap;

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
pub enum GbaEvent {
    HDraw,
    HBlank,
    VBlankHDraw,
    VBlankHBlank,
    UpdateVCount,
}

pub struct Scheduler {
    timestamp_now: u64,
    heap: BinaryHeap<SchedulerEvent>,
}

impl Scheduler {
    pub fn new(capacity: usize) -> Self {
        Self {
            timestamp_now: 0,
            heap: BinaryHeap::with_capacity(capacity),
        }
    }

    pub fn get_timestamp(&self) -> u64 {
        self.timestamp_now
    }

    pub fn step(&mut self, cycles: u8) {
        self.timestamp_now += u64::from(cycles);
    }

    pub fn add(&mut self, cycles: u32, gba_event: GbaEvent) {
        let timestamp = self.timestamp_now + u64::from(cycles);
        self.heap.push(SchedulerEvent {
            timestamp,
            gba_event,
        });
    }

    pub fn poll_event(&mut self) -> Option<GbaEvent> {
        let Some(event) = self.heap.peek() else {
            return None;
        };

        if event.timestamp <= self.timestamp_now {
            let gba_event = event.gba_event;
            self.heap.pop();
            Some(gba_event)
        } else {
            None
        }
    }

    pub fn clear(&mut self) {
        self.timestamp_now = 0;
        self.heap.clear();
    }
}

#[derive(Clone, Copy, Eq, PartialEq, Debug)]
struct SchedulerEvent {
    timestamp: u64,
    gba_event: GbaEvent,
}

impl Ord for SchedulerEvent {
    fn cmp(&self, other: &Self) -> Ordering {
        other.timestamp.cmp(&self.timestamp)
    }
}

impl PartialOrd for SchedulerEvent {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_scheduler() {
        let mut scheduler = Scheduler::new(32);
        scheduler.add(0, GbaEvent::VBlankHDraw);
        scheduler.add(10, GbaEvent::HDraw);
        scheduler.add(10, GbaEvent::VBlankHBlank);
        scheduler.add(5, GbaEvent::HBlank);
        scheduler.add(8, GbaEvent::UpdateVCount);

        assert_eq!(scheduler.poll_event(), Some(GbaEvent::VBlankHDraw));
        assert_eq!(scheduler.poll_event(), None);

        scheduler.step(5);

        assert_eq!(scheduler.poll_event(), Some(GbaEvent::HBlank));
        assert_eq!(scheduler.poll_event(), None);

        scheduler.step(5);

        assert_eq!(scheduler.poll_event(), Some(GbaEvent::UpdateVCount));
        assert_eq!(scheduler.poll_event(), Some(GbaEvent::HDraw));
        assert_eq!(scheduler.poll_event(), Some(GbaEvent::VBlankHBlank));
        assert_eq!(scheduler.poll_event(), None);
    }
}
