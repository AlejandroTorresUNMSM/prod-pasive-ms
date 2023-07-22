package com.atorres.nttdata.prodpasivems.exception;

import lombok.Builder;
import lombok.Data;
import org.springframework.http.HttpStatus;

@Data
@Builder
public class ErrorDto {
    private HttpStatus httpStatus;
    private String message;
}
