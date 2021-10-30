use std::io::{self, BufRead,Write};
use std::env;

struct Date {
    year: usize,
    month: u8,
    day: u8,
    hour: u8,
    minute: u8,
}

impl Date {
    fn my_print(&self) -> () {
        print!("{:>0wid2$}/{:>0wid2$}/{} @ {:>0wid2$}:{:>0wid2$}", self.month, self.day, self.year, self.hour, self.minute, wid2=2);
        std::io::stdout().flush().unwrap();
    }

    fn my_read(date: String) -> Date {
         Date {
            year: date[0..4].parse::<usize>().unwrap(),
            month: date[4..6].parse::<u8>().unwrap(),
            day: date[6..8].parse::<u8>().unwrap(),
            hour: date[9..11].parse::<u8>().unwrap(),
            minute: date[11..13].parse::<u8>().unwrap(),
        }
    }

}


fn main() {

    let stdin = io::stdin();
    let args: Vec<_> = env::args().collect();

    let mut utc_hour: u8 = 0;
    let mut utc_minute: u8 = 0;

    if args.len() > 1 && args[1]=="--offset" {
        utc_hour = String::from(&args[2][0..2]).parse::<u8>().unwrap(); //might need to go further after
        utc_minute = String::from(&args[2][2..4]).parse::<u8>().unwrap();
    }

    for line in stdin.lock().lines() {
        let l = String::from(line.unwrap());
        let v = l.split("|").collect::<Vec<&str>>();

        let mut scheduled = Date::my_read(v[0].to_string());
        scheduled.hour += utc_hour;
        scheduled.minute += utc_minute;
        scheduled.my_print();

        print!(" -> ");
        std::io::stdout().flush().unwrap();

        let mut until = Date::my_read(v[1].to_string());
        until.hour += utc_hour;
        until.minute += utc_minute;
        until.my_print();

        println!("|{}", v[2]);

    }
}
